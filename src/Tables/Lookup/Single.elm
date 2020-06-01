module Tables.Lookup.Single exposing (Single, decode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (Tag, int16, offset16, offset32, tag, uint16)


type Single
    = Format1 { coverageOffset : Int, deltaGlyphID : Int }
    | Format2 { coverageOffset : Int, substituteGlyphIDs : Array Int }


substitute : Single -> Int -> Int
substitute table glyphID =
    case table of
        Format1 { deltaGlyphID } ->
            (glyphID + deltaGlyphID) |> modBy 65536

        Format2 { substituteGlyphIDs } ->
            -1


decode : Decoder Single
decode =
    uint16
        |> Decode.andThen
            (\format ->
                case format of
                    1 ->
                        Decode.map2 (\coverageOffset deltaGlyphID -> Format1 { coverageOffset = coverageOffset, deltaGlyphID = deltaGlyphID })
                            offset16
                            int16

                    2 ->
                        Decode.map2 (\coverageOffset subsituteGlyphIDs -> Format2 { coverageOffset = coverageOffset, substituteGlyphIDs = subsituteGlyphIDs })
                            offset16
                            (Decode.Extra.sizedArray uint16 uint16)

                    _ ->
                        Decode.fail
            )
