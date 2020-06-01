module Woff2 exposing (decode)

import Array exposing (Array)
import Bitwise
import Brotli
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra as Decode
import Dict exposing (Dict)
import Font exposing (Font)
import OpenType

decode : Bytes -> Maybe Font
decode buffer =
    case Decode.decode decoder buffer of
        Nothing ->
            Nothing

        Just ( _, tables ) ->
            Just { tables = tables }


decoder : Decoder ( Woff2Header, Dict String Bytes )
decoder =
    woff2Header
        |> Decode.andThen
            (\header ->
                Decode.map3 (\a _ b -> ( a, b )) (tableEntries header.numTables) (skipCollection header.flavor) (Decode.bytes header.totalCompressedSize)
                    |> Decode.andThen
                        (\( myTableEntries, buffer ) ->
                            case Brotli.decode buffer of
                                Err e ->
                                    Decode.fail

                                Ok decompressedData ->
                                    case OpenType.splitIntoTables decompressedData (List.map extractFromTableDirectoryEntry myTableEntries) of
                                        Nothing ->
                                            Decode.fail

                                        Just tables ->
                                            Decode.succeed ( header, tables )
                        )
            )


{-| Woff2 files can store collections of fonts. Font collections have a special 'flavor', and have some extra data after the header and tables.
We currently ignore the collection aspect, so this will just skip the collection data, and proceed to read the first font in the file.
-}
skipCollection : Int -> Decoder ()
skipCollection flavor =
    if flavor == 0x74746366 then
        let
            helper remaining =
                if remaining > 0 then
                    Decode.map2 Tuple.pair decode255UShort uint32
                        |> Decode.andThen (\( numTables2, flavor2 ) -> Decode.array numTables2 decode255UShort)
                        |> Decode.map (\_ -> Decode.Loop (remaining - 1))

                else
                    Decode.succeed (Decode.Done ())
        in
        Decode.map2 Tuple.pair uint32 decode255UShort
            |> Decode.andThen (\( version, numFonts ) -> Decode.loop numFonts helper)

    else
        Decode.succeed ()


uint32 : Decoder Int
uint32 =
    Decode.unsignedInt32 BE


uint16 : Decoder Int
uint16 =
    Decode.unsignedInt16 BE


uint8 : Decoder Int
uint8 =
    Decode.unsignedInt8


uintBase128 : Decoder Int
uintBase128 =
    let
        loopHelp ( i, accum ) =
            if i < 5 then
                uint8
                    |> Decode.andThen
                        (\data_byte ->
                            if i == 0 && data_byte == 0x80 then
                                -- no leading 0s
                                Decode.fail

                            else if Bitwise.and accum 0xFE000000 /= 0 then
                                -- If any of top 7 bits are set then << 7 would overflow
                                Decode.fail

                            else
                                let
                                    newAccum =
                                        Bitwise.or (Bitwise.shiftLeftBy 7 accum) (Bitwise.and data_byte 0x7F)
                                in
                                if Bitwise.and data_byte 0x80 == 0 then
                                    Decode.succeed (Done newAccum)

                                else
                                    Decode.succeed (Loop ( i + 1, newAccum ))
                        )

            else
                --  UIntBase128 sequence exceeds 5 bytes
                Decode.fail
    in
    Decode.loop ( 0, 0 ) loopHelp


decode255UShort : Decoder Int
decode255UShort =
    let
        oneMoreByteCode1 =
            255

        oneMoreByteCode2 =
            254

        wordCode =
            253

        lowestUCode =
            253
    in
    uint8
        |> Decode.andThen
            (\code ->
                if code == wordCode then
                    Decode.map2
                        (\b1 b2 ->
                            let
                                v1 =
                                    Bitwise.and 0xFF00 (Bitwise.shiftLeftBy 8 b1)

                                v2 =
                                    b2
                            in
                            Bitwise.or v1 (Bitwise.and v2 0xFF)
                        )
                        uint8
                        uint8

                else if code == oneMoreByteCode1 then
                    Decode.map (\b1 -> b1 + lowestUCode) uint8

                else if code == oneMoreByteCode2 then
                    Decode.map (\b1 -> b1 + lowestUCode * 2) uint8

                else
                    Decode.succeed code
            )


type alias Woff2Header =
    { signature : Int
    , flavor : Int
    , length : Int
    , numTables : Int
    , reserved : Int
    , totalSfntSize : Int
    , totalCompressedSize : Int
    , majorVersion : Int
    , minorVersion : Int
    , metaOffset : Int
    , metaLength : Int
    , metaOrigLength : Int
    , privOffset : Int
    , privLength : Int
    }


woff2Header : Decoder Woff2Header
woff2Header =
    Decode.succeed Woff2Header
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint16
        |> Decode.andMap uint16
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint16
        |> Decode.andMap uint16
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint32
        |> Decode.andMap uint32


type alias TableDirectoryEntry =
    { name : String, transform : Int, origLength : Int, transformLength : Int, offset : Int }


extractFromTableDirectoryEntry : TableDirectoryEntry -> { name : String, offset : Int }
extractFromTableDirectoryEntry { name, offset } =
    { name = name, offset = offset }


tableEntries : Int -> Decoder (List TableDirectoryEntry)
tableEntries n =
    Decode.loop { remaining = n, offset = 0, accum = [] } tablesHelp


tablesHelp :
    { offset : Int, accum : List TableDirectoryEntry, remaining : Int }
    -> Decoder (Step { offset : Int, accum : List TableDirectoryEntry, remaining : Int } (List TableDirectoryEntry))
tablesHelp { offset, accum, remaining } =
    let
        decodeFlags =
            uint8
                |> Decode.andThen
                    (\v ->
                        let
                            flags =
                                Bitwise.and v 0x1F
                        in
                        if flags == 0x3F then
                            uint32 |> Decode.map String.fromInt

                        else
                            Array.get flags knownTableTags
                                |> Maybe.withDefault ""
                                |> Decode.succeed
                    )

        decodeTransformLength tag origLength =
            if tag == "glyf" || tag == "loca" then
                uintBase128

            else
                Decode.succeed origLength
    in
    if remaining > 0 then
        decodeFlags
            |> Decode.andThen
                (\tag ->
                    uintBase128
                        |> Decode.andThen
                            (\origLength ->
                                decodeTransformLength tag origLength
                                    |> Decode.map
                                        (\transformLength ->
                                            Decode.Loop
                                                { remaining = remaining - 1
                                                , offset = offset + transformLength
                                                , accum =
                                                    { name = tag
                                                    , transform = -1
                                                    , origLength = origLength
                                                    , transformLength = transformLength
                                                    , offset = offset
                                                    }
                                                        :: accum
                                                }
                                        )
                            )
                )

    else
        Decode.succeed (Decode.Done (List.reverse accum))


knownTableTags : Array String
knownTableTags =
    Array.fromList
    [ "cmap"
    , "head"
    , "hhea"
    , "hmtx"
    , "maxp"
    , "name"
    , "OS/2"
    , "post"
    , "cvt "
    , "fpgm"
    , "glyf"
    , "loca"
    , "prep"
    , "CFF "
    , "VORG"
    , "EBDT"
    , "EBLC"
    , "gasp"
    , "hdmx"
    , "kern"
    , "LTSH"
    , "PCLT"
    , "VDMX"
    , "vhea"
    , "vmtx"
    , "BASE"
    , "GDEF"
    , "GPOS"
    , "GSUB"
    , "EBSC"
    , "JSTF"
    , "MATH"
    , "CBDT"
    , "CBLC"
    , "COLR"
    , "CPAL"
    , "SVG "
    , "sbix"
    , "acnt"
    , "avar"
    , "bdat"
    , "bloc"
    , "bsln"
    , "cvar"
    , "fdsc"
    , "feat"
    , "fmtx"
    , "fvar"
    , "gvar"
    , "hsty"
    , "just"
    , "lcar"
    , "mort"
    , "morx"
    , "opbd"
    , "prop"
    , "trak"
    , "Zapf"
    , "Silf"
    , "Glat"
    , "Gloc"
    , "Feat"
    , "Sill"
    ]
