module Tables.CharacterMapping exposing (CharacterMapping, Encoding(..), decode, getGlyphIndex, maximumNumberOfGlyphs)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (int16, uint16)


type alias CharacterMapping =
    { header : Header

    --, records : Array Record
    , encodings : Array Encoding
    }


type Encoding
    = Format0 Encoding0
    | Format4 Encoding4
    | Format6 Encoding6
    | Unknown Int


maximumNumberOfGlyphs : CharacterMapping -> Int -> Int
maximumNumberOfGlyphs cmap index =
    case cmap.encodings |> Array.get index of
        Nothing ->
            0

        Just (Format6 encoding) ->
            Array.length encoding.glyphIdArray

        Just (Format4 encoding) ->
            {-
               List.map2 (\start end -> end - start) (Array.toList encoding.startCode) (Array.toList encoding.endCode)
                   |> List.sum
            -}
            encoding.searchRange

        Just (Format0 _) ->
            256

        Just (Unknown _) ->
            0


getGlyphIndex : CharacterMapping -> Int -> Char -> Int
getGlyphIndex cmap index char =
    case cmap.encodings |> Array.get index of
        Just (Format6 encoding) ->
            Array.get (Char.toCode char - encoding.firstCode) encoding.glyphIdArray
                |> Maybe.withDefault 0

        Just (Format4 encoding) ->
            let
                constructor start end delta offset =
                    { start = start, end = end, delta = delta, offset = offset }

                ranges =
                    List.map4 constructor
                        (Array.toList encoding.startCode)
                        (Array.toList encoding.endCode)
                        (Array.toList encoding.idDelta)
                        (Array.toList encoding.idRangeOffset)
                        |> List.indexedMap Tuple.pair

                go character list =
                    case list of
                        [] ->
                            Nothing

                        ( i, { start, end } as range ) :: rest ->
                            if character >= start && character <= end then
                                Just ( i, range )

                            else
                                go character rest

                maybeSegmentIndex =
                    go (Char.toCode char) ranges
            in
            case maybeSegmentIndex of
                Just ( segmentIndex, range ) ->
                    Maybe.withDefault 0 <|
                        if range.offset == 0 then
                            Just (Char.toCode char + range.delta |> modBy 65536)

                        else
                            let
                                -- the range.offset is given relative to the current position in the idRangeOffset buffer.
                                -- we need some math to get the correct index in the glyphIdArray.
                                --
                                -- so with `2 * segmentIndex + range.offset` we get an offset (in bytes) relative to the start of the idRangeOffset array.
                                -- this values divided by two to get the index (all elements are 16-bit integers, i.e. 2 bytes per element in the array)
                                -- then we subtract the length of idRangeOffset to get an index into glyphIdArray
                                -- and finally add the difference within the block (difference with the starting character of the block)
                                glyphIndex =
                                    ((2 * segmentIndex + range.offset) // 2) - Array.length encoding.idRangeOffset + (Char.toCode char - range.start)
                            in
                            Array.get glyphIndex encoding.glyphIdArray
                                |> Maybe.map (\v -> range.delta + v |> modBy 65536)

                Nothing ->
                    0

        Just (Format0 encoding) ->
            Array.get (Char.toCode char) encoding.glyphIdArray
                |> Maybe.withDefault 0

        Just _ ->
            Debug.log "different encoding found" 0

        Nothing ->
            Debug.log "encoding not found" 0


decode : Bytes -> Decoder CharacterMapping
decode bytes =
    let
        records numTables =
            Decode.Extra.array numTables decodeEncodingRecord

        encoding { offset } =
            case Decode.Extra.dropLeft offset bytes of
                Nothing ->
                    Decode.fail

                Just newBytes ->
                    case Decode.decode decodeEncoding newBytes of
                        Nothing ->
                            Decode.fail

                        Just v ->
                            Decode.succeed v

        decodeEncoding =
            Decode.unsignedInt16 BE
                |> Decode.andThen
                    (\version ->
                        case version of
                            0 ->
                                Decode.map Format0 version0

                            4 ->
                                Decode.map Format4 version4

                            6 ->
                                Decode.map Format6 version6

                            format ->
                                Decode.succeed (Unknown format)
                    )

        encodings : Int -> Decoder (Array Encoding)
        encodings numTables =
            records numTables
                |> Decode.map (Array.toList << Array.map encoding)
                |> Decode.andThen (Decode.Extra.mapM identity)
                |> Decode.map Array.fromList

        --|> Decode.map (Array.map (Debug.log "cmap encoding"))
    in
    decodeCharacterMapHeader
        |> Decode.andThen
            (\header ->
                Decode.succeed (CharacterMapping header)
                    -- |> andMap (records header.numTables)
                    |> andMap (encodings header.numTables)
            )


type alias Record =
    { platformID : Int, encodingID : Int, offset : Int }


decodeEncodingRecord =
    Decode.succeed Record
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt32 BE)


type alias Header =
    { version : Int, numTables : Int }


decodeCharacterMapHeader : Decoder Header
decodeCharacterMapHeader =
    Decode.succeed Header
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)


type alias Encoding0 =
    { length : Int
    , language : Int
    , glyphIdArray : Array Int
    }


version0 =
    Decode.succeed Encoding0
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.Extra.exactly 256 Decode.unsignedInt8 |> Decode.map Array.fromList)


type alias Encoding4 =
    { length : Int
    , language : Int
    , segCountX2 : Int
    , searchRange : Int
    , entrySelector : Int
    , rangeShift : Int
    , endCode : Array Int
    , reservedPad : Int
    , startCode : Array Int
    , idDelta : Array Int
    , idRangeOffset : Array Int
    , glyphIdArray : Array Int
    }


version4 =
    Decode.map3 (\a b c -> ( a, b, c )) uint16 uint16 uint16
        |> Decode.andThen
            (\( length, language, segCountX2 ) ->
                let
                    segCount =
                        segCountX2 // 2

                    bytesUsed =
                        (8 + 4 * segCount) * 2

                    glyphIdArrayLength =
                        ((length - bytesUsed) // 2) |> Debug.log "=> glyph array length"
                in
                Decode.succeed (Encoding4 length language segCountX2)
                    |> andMap uint16
                    |> andMap uint16
                    |> andMap uint16
                    |> andMap (Decode.Extra.array segCount uint16)
                    |> andMap uint16
                    |> andMap (Decode.Extra.array segCount uint16)
                    |> andMap (Decode.Extra.array segCount int16)
                    |> andMap (Decode.Extra.array segCount uint16)
                    |> andMap (Decode.Extra.array glyphIdArrayLength uint16)
            )


type alias Encoding6 =
    { length : Int
    , language : Int
    , firstCode : Int
    , entryCount : Int
    , glyphIdArray : Array Int
    }


version6 =
    let
        handler length language firstCode entryCount =
            Decode.Extra.array entryCount (Decode.unsignedInt16 BE)
                |> Decode.map
                    (\glyphIdArray ->
                        Encoding6 length language firstCode entryCount glyphIdArray
                    )

        join =
            Decode.andThen identity
    in
    Decode.succeed handler
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> join
