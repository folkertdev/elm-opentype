module Tables.HorizontalHeader exposing (HorizontalHeader, decode)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra exposing (keep)
import Decode.Opentype exposing (fword, int16, ufword, uint16)


type alias HorizontalHeader =
    { majorVersion : Int
    , minorVersion : Int
    , ascender : Int
    , descender : Int
    , lineGap : Int
    , advanceWidthMax : Int
    , minLeftSideBearing : Int
    , minRightSideBearing : Int
    , xMaxExtent : Int
    , caretSlopeRise : Int
    , caretSlopeRun : Int
    , caretOffset : Int
    , reserved1 : Int
    , reserved2 : Int
    , reserved3 : Int
    , reserved4 : Int
    , metricDataFormat : Int
    , numberOfHMetrics : Int
    }


decode : Decoder HorizontalHeader
decode =
    Decode.succeed HorizontalHeader
        |> keep uint16
        |> keep uint16
        |> keep fword
        |> keep fword
        |> keep fword
        |> keep ufword
        |> keep fword
        |> keep fword
        |> keep fword
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep int16
        |> keep uint16
