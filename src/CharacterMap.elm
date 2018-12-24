module CharacterMap exposing (decode)

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Utils exposing (keep)


type alias CharacterMap =
    { header : CharacterMapHeader }


decode =
    Decode.succeed CharacterMap
        |> keep decodeCharacterMapHeader


type alias EncodingRecord =
    { platformID : Int, encodingID : Int, offset : Int }


decodeEncodingRecord =
    Decode.succeed EncodingRecord
        |> keep (Decode.unsignedInt16 BE)
        |> keep (Decode.unsignedInt16 BE)
        |> keep (Decode.unsignedInt32 BE)


type alias CharacterMapHeader =
    { version : Int, numTables : Int, encodingRecords : Array EncodingRecord }


decodeCharacterMapHeader : Decoder CharacterMapHeader
decodeCharacterMapHeader =
    Decode.succeed Tuple.pair
        |> keep (Decode.unsignedInt16 BE)
        |> keep (Decode.unsignedInt16 BE)
        |> Decode.andThen
            (\( version, numTables ) ->
                Decode.map (\encodingRecords -> CharacterMapHeader version numTables (Array.fromList encodingRecords)) (Utils.exactly numTables decodeEncodingRecord)
            )
