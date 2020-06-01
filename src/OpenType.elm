module OpenType exposing (decode, splitIntoTables)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra as Decode
import Decode.Opentype exposing (Tag)
import Dict exposing (Dict)
import Font exposing (Font)

type alias OffsetTable =
    { sfntVersion : Tag
    , numTables : Int
    , searchRange : Int
    , entrySelector : Int
    , rangeShift : Int
    }


decode : Bytes -> Maybe Font
decode bytes =
    Decode.decode (decoder bytes) bytes


decoder : Bytes -> Decoder Font
decoder bytes =
    offsetTableDecoder
        |> Decode.andThen
            (\fontHeader ->
                tableRecordsDecoder fontHeader.numTables
                    |> Decode.andThen
                        (\tableRecords ->
                            case splitIntoTables bytes (List.map extractFromTableRecord tableRecords) of
                                Nothing ->
                                    Decode.fail

                                Just v ->
                                    Decode.succeed (Font v)
                        )
            )


offsetTableDecoder =
    Decode.succeed OffsetTable
        |> Decode.andMap Decode.Opentype.tag
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)


type alias TableRecord =
    { tag : String
    , checkSum : Int
    , offset : Int
    , length : Int
    }


extractFromTableRecord : TableRecord -> { name : String, offset : Int }
extractFromTableRecord record =
            { name = record.tag, offset = record.offset }


splitIntoTables : Bytes ->  List { name : String, offset : Int } -> Maybe (Dict String Bytes)
splitIntoTables buffer tableRecords =
    let
        sortedRecords = List.sortBy .offset tableRecords

        records =
            { name = "", offset = 0 } :: (sortedRecords ++ [ { name = "", offset = Bytes.width buffer } ])

        pairs =
            List.map2 (\r1 r2 -> { name = r1.name, start = r1.offset, end = r2.offset }) records (List.drop 1 records)

        elementDecoder { name, start, end } =
            Decode.bytes (end - start)
                |> Decode.map (\bytes -> ( name, bytes ))

        sliceDecoder =
            Decode.mapM elementDecoder pairs
                |> Decode.map (Dict.fromList << List.drop 1)
    in
    Decode.decode sliceDecoder buffer


tableRecordDecoder =
    Decode.succeed TableRecord
        |> Decode.andMap Decode.Opentype.tagAsString
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)




tableRecordsDecoder : Int -> Decoder (List TableRecord)
tableRecordsDecoder tableCount =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n - 1, x :: xs )) tableRecordDecoder
    in
    Decode.loop ( tableCount, [] ) helper
