module Tables.HorizontalMetrics exposing (HorizontalMetrics, decode)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra exposing (andMap)


type alias HorizontalMetrics =
    { horizontalMetrics : Array HorizontalMetric
    , leftSideBearings : Array Int
    }


decode : { numberOfHMetrics : Int, numberOfGlyphs : Int } -> Decoder HorizontalMetrics
decode { numberOfHMetrics, numberOfGlyphs } =
    Decode.succeed HorizontalMetrics
        |> andMap (Decode.Extra.array numberOfHMetrics decodeHorizontalMetric)
        |> andMap (Decode.Extra.array (numberOfGlyphs - numberOfHMetrics) (Decode.unsignedInt16 BE))


type alias HorizontalMetric =
    { advanceWidth : Int, leftSideBearing : Int }


decodeHorizontalMetric =
    Decode.succeed HorizontalMetric
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.signedInt16 BE)
