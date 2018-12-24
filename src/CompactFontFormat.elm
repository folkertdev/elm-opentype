module CompactFontFormat exposing (Cff, decode, glyphs, parse, tryFirst)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charset exposing (Charset)
import Charstring.Internal as Charstring exposing (Charstring, Operation, Segment)
import Dict exposing (Dict)
import Dict.Private exposing (Private)
import Dict.Top exposing (Top)
import Encoding exposing (Encodings)
import Glyphs exposing (Glyphs)
import Index exposing (OffsetSize)
import Utils exposing (drop, keep)


type alias Header =
    { major : Int
    , minor : Int
    , headerSize : Int
    , offsetSize : OffsetSize
    }


decodeHeader =
    Decode.map4 Header
        Decode.unsignedInt8
        Decode.unsignedInt8
        Decode.unsignedInt8
        Index.offSize


type alias Cff =
    { header : Header
    , names : List String
    , tops : Array Top
    , strings : List String
    , subroutines : Array (List Segment)
    }


parse : Bytes -> Maybe Cff
parse bytes =
    Decode.decode decode bytes


decode =
    Decode.succeed Cff
        |> keep decodeHeader
        |> keep Index.name
        |> keep (Decode.map Array.fromList Index.top)
        |> keep Index.string
        |> keep (Decode.map Array.fromList Index.globalSubRoutines)


tryFirst : Cff -> Maybe Charstring
tryFirst cff =
    case Array.get 0 cff.subroutines of
        Nothing ->
            Nothing

        Just sub ->
            -- Charstring.decodeWithOptions { global = cff.subroutines, local = Nothing } sub
            -- TODO fix this
            Nothing


glyphs : Int -> Bytes -> Cff -> Int -> Maybe Glyphs
glyphs offset bytes cff index =
    Array.get index cff.tops
        |> Maybe.andThen (\top -> Glyphs.parse offset bytes top cff.subroutines)
