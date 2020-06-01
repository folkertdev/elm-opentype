module Tables.Header exposing (BoundingBox, Header, decode)

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (LongDateTime, Tag, Version, fixed, int16, longDateTime, offset16, offset32, tag, uint16, uint32)


type alias Header =
    { version : Version
    , fontRevision : Int
    , checkSumAdjustment : Int
    , magicNumber : Int
    , flags : Int
    , unitsPerEm : Int
    , created : LongDateTime
    , modified : LongDateTime
    , boundingBox : BoundingBox
    , macStyle : Int
    , lowestRecPPEM : Int
    , fontDirectionHint : Int
    , indexToLocFormat : Int
    , glyphDataFormat : Int
    }


decode : Decoder Header
decode =
    Decode.succeed Header
        |> andMap Decode.Opentype.version
        |> andMap fixed
        |> andMap uint32
        |> andMap uint32
        |> andMap uint16
        |> andMap uint16
        |> andMap longDateTime
        |> andMap longDateTime
        |> andMap decodeBoundingBox
        |> andMap uint16
        |> andMap uint16
        |> andMap uint16
        |> andMap uint16
        |> andMap uint16


type alias BoundingBox =
    { xMin : Int, yMin : Int, xMax : Int, yMax : Int }


decodeBoundingBox =
    Decode.succeed BoundingBox
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
