module Woff exposing (decode)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra as Decode
import Decode.Opentype
import Dict
import Font exposing (Font)
import Inflate


decode : Bytes -> Maybe Font
decode buffer =
    case Decode.decode decoder buffer of
        Nothing ->
            Nothing

        Just ( _, entries ) ->
            let
                tables =
                    entries
                        |> Array.toList
                        |> List.filterMap (processEntry buffer)
                        |> Dict.fromList
            in
            Just { tables = tables }


processEntry : Bytes -> TableDirectoryEntry -> Maybe ( String, Bytes )
processEntry buffer entry =
    let
        maybeInflate =
            if entry.compLength /= entry.origLength then
                Inflate.inflate

            else
                Just

        newBuffer =
            slice entry.offset entry.compLength buffer
                |> Maybe.andThen maybeInflate
    in
    Maybe.map2 Tuple.pair (Just entry.tag) newBuffer


slice : Int -> Int -> Bytes -> Maybe Bytes
slice from size buffer =
    Decode.decode (Decode.map2 (\_ v -> v) (Decode.bytes from) (Decode.bytes size)) buffer


decoder : Decoder ( WoffHeader, Array TableDirectoryEntry )
decoder =
    woffHeader
        |> Decode.andThen
            (\header ->
                Decode.array header.numTables tableDirectoryEntry
                    |> Decode.map
                        (\entries ->
                            ( header, entries )
                        )
            )


type alias TableDirectoryEntry =
    { tag : String, offset : Int, compLength : Int, origLength : Int, origChecksum : Int }


tableDirectoryEntry : Decoder TableDirectoryEntry
tableDirectoryEntry =
    Decode.succeed TableDirectoryEntry
        |> Decode.andMap Decode.Opentype.tagAsString
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)


type alias WoffHeader =
    { signature : Int
    , flavor : Int
    , length : Int
    , numTables : Int
    , reserved : Int
    , totalSfntSize : Int
    , majorVersion : Int
    , minorVersion : Int
    , metaOffset : Int
    , metaLength : Int
    , metaOrigLength : Int
    , privOffset : Int
    , privLength : Int
    }


woffHeader : Decoder WoffHeader
woffHeader =
    Decode.succeed WoffHeader
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
