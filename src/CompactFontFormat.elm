module CompactFontFormat exposing (CFF, charstring, decode)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Internal as Charstring exposing (Charstring, Operation)
import Dict exposing (Dict)
import Operator exposing (DICT)


type alias CFF =
    { header : Header
    , names : List String
    , top : List DICT
    , strings : List String
    , globalSubRoutines : List ()
    , encoding : ( Encoding, List ( Int, SID ) )
    , charset : Charset
    , charstrings : List Charstring
    }


decode : Decoder CFF
decode =
    {-
       Decode.bytes 4
           |> Decode.andThen
               (\bytes ->
                   case Decode.decode header bytes of
                       Nothing ->
                           Decode.fail

                       Just v ->
                           v
                               |> Debug.log "header"
                               |> Decode.succeed
               )
           |> Decode.andThen
               (\h ->
                   Decode.string 1
                       -- h.headerSize
                       |> Decode.andThen
                           (\_ ->
                               Decode.map (\n -> CFF h n) name
                           )
               )
    -}
    Decode.succeed CFF
        |> keep header
        |> keep name
        |> keep top
        |> keep (Decode.map (\_ -> []) strings)
        |> keep (Decode.map (\_ -> []) globalSubRoutines)
        |> keep encoding
        {-
           |> keep (Decode.map (\_ -> []) (charset 1482))
           |> keep charstring
        -}
        |> keep (Decode.succeed [])
        |> keep (Decode.succeed [])



{-
   index : Decode.Decoder a -> Decode.Decoder (List a)
   index =
       card16
           |> Decode.andThen
               (\count ->
                   if count == 0 then
                       Decode.succeed []

                   else
                       decodeSized
               )
-}


sizedIndex : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndex elementDecoder =
    card16
        |> Decode.andThen
            (\count ->
                if Debug.log "count" count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                let
                                    _ =
                                        -- Debug.log "offsetSize" offsetSize
                                        ()
                                in
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                _ =
                                                    -- Debug.log "offsets" offsets
                                                    ()

                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)

                                                -- |> Debug.log "deltas"
                                            in
                                            mapM elementDecoder deltas
                                        )
                            )
            )


type alias Header =
    { major : Int
    , minor : Int
    , headerSize : Int
    , offsetSize : OffsetSize
    }


header =
    Decode.succeed Header
        |> keep card8
        |> keep card8
        |> keep card8
        |> keep offSize


name : Decoder (List String)
name =
    sizedIndex Decode.string


strings : Decoder (List String)
strings =
    sizedIndex Decode.string


globalSubRoutines : Decoder (List String)
globalSubRoutines =
    sizedIndex Decode.string


charstring : Decoder (List Charstring)
charstring =
    -- sizedIndex Decode.string
    sizedIndex (\_ -> Charstring.decode)


type Encoding
    = Glyphs (List Int)
    | Ranges (List ( Int, Int ))


type alias Range1 =
    { first : Int, nLeft : Int }


encoding : Decoder ( Encoding, List ( Int, SID ) )
encoding =
    let
        range1 =
            Decode.succeed Tuple.pair
                |> keep card8
                |> keep card8

        glyph =
            card8

        sups =
            Decode.succeed Tuple.pair
                |> keep card8
                |> keep sid
    in
    card8
        |> Decode.andThen
            (\version ->
                case version of
                    0 ->
                        Decode.unsignedInt8
                            |> Decode.andThen
                                (\nCodes ->
                                    exactly nCodes glyph
                                        |> Decode.map Glyphs
                                )

                    1 ->
                        Decode.unsignedInt8
                            |> Decode.andThen
                                (\nRanges ->
                                    exactly nRanges range1
                                        |> Decode.map Ranges
                                )

                    2 ->
                        Decode.unsignedInt8
                            |> Decode.andThen
                                (\nCodes ->
                                    exactly nCodes glyph
                                        |> Decode.map Glyphs
                                )

                    _ ->
                        let
                            _ =
                                Debug.log "invalid encoding version" version
                        in
                        Decode.fail
            )
        |> Decode.andThen
            (\mappings ->
                Decode.unsignedInt8
                    |> Decode.andThen
                        (\nSups ->
                            exactly nSups sups
                                |> Decode.andThen
                                    (\currentSups ->
                                        Decode.succeed ( mappings, currentSups )
                                    )
                        )
            )


type alias Charset =
    List SID


charset : Int -> Decoder Charset
charset nGlyphs =
    card8
        |> Decode.andThen
            (\version ->
                case version of
                    0 ->
                        exactly (nGlyphs - 1) sid

                    1 ->
                        Debug.todo "no idea what is going on here"

                    2 ->
                        Debug.todo "no idea what is going on here either"

                    210 ->
                        exactly (nGlyphs - 1) sid

                    _ ->
                        let
                            _ =
                                Debug.log "invalid charset version" version
                        in
                        Decode.fail
            )


top : Decoder (List DICT)
top =
    card16
        |> Decode.andThen
            (\count ->
                if Debug.log "count" count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                let
                                    _ =
                                        Debug.log "offsetSize" offsetSize
                                in
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                _ =
                                                    Debug.log "offsets" offsets

                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                                        |> Debug.log "deltas"
                                            in
                                            mapM (\_ -> Operator.decode) deltas
                                        )
                            )
            )


dict : Int -> Decoder DICT
dict _ =
    Operator.decode


card8 =
    Decode.unsignedInt8


card16 =
    Decode.unsignedInt16 BE


type OffsetSize
    = Bytes1
    | Bytes2
    | Bytes3
    | Bytes4


offSize =
    Decode.unsignedInt8
        |> Decode.andThen
            (\size ->
                case size of
                    1 ->
                        Decode.succeed Bytes1

                    2 ->
                        Decode.succeed Bytes2

                    3 ->
                        Decode.succeed Bytes3

                    4 ->
                        Decode.succeed Bytes4

                    _ ->
                        let
                            _ =
                                Debug.log "invalid offset size " size
                        in
                        Decode.fail
            )


offset : OffsetSize -> Decoder Int
offset size =
    case size of
        Bytes1 ->
            Decode.unsignedInt8

        Bytes2 ->
            Decode.unsignedInt16 BE

        Bytes3 ->
            -- TODO check this logic
            Decode.succeed (\small large -> small + Bitwise.shiftLeftBy 16 large)
                |> keep (Decode.unsignedInt16 BE)
                |> keep Decode.unsignedInt8

        Bytes4 ->
            Decode.unsignedInt32 BE



{-
   topDictDataEntry : Int -> Decoder (Dict String ())
   topDictDataEntry operatorFirstByte =
       case operatorFirstByte of
           0 ->
               Decode.map (Dict.insert "version") sid

           1 ->
               Decode.map (Dict.insert "Notice") sid

           2 ->
               5
                   Decode.map
                   (Dict.insert "FullName")
                   sid

           3 ->
               Decode.map (Dict.insert "FamilyName") sid

           4 ->
               Decode.map (Dict.insert "Weight") sid
-}


type alias SID =
    String


sid : Decoder SID
sid =
    Decode.string 2


andMap first later =
    Decode.map2 (<|) later first


keep =
    andMap


skip drop preserve =
    Decode.map2 (\_ p -> p) drop preserve


mapM : (a -> Decoder b) -> List a -> Decoder (List b)
mapM f xs =
    Decode.loop ( [], xs ) (mapMHelp f)


mapMHelp : (a -> Decoder b) -> ( List b, List a ) -> Decoder (Step ( List b, List a ) (List b))
mapMHelp elementDecoder ( accum, remaining ) =
    case remaining of
        first :: rest ->
            Decode.map (\new -> Loop ( new :: accum, rest )) (elementDecoder first)

        [] ->
            Decode.succeed (Done (List.reverse accum))


list : Decoder a -> Decoder (List a)
list decoder =
    Decode.unsignedInt32 BE
        |> Decode.andThen (\len -> Decode.loop ( len, [] ) (listStep decoder))


exactly : Int -> Decoder a -> Decoder (List a)
exactly len decoder =
    Decode.loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Done (List.reverse xs))

    else
        Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
