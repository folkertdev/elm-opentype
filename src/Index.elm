module Index exposing (OffsetSize, charstring, charstringWithOptions, globalSubRoutines, localSubRoutines, name, offSize, string, top)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Internal as Charstring exposing (Charstring, Operation, Segment)
import Dict.Top exposing (Top)


top : Decoder (List Top)
top =
    sizedIndex Dict.Top.decode



{-
   sizedIndex Decode.string
       |> Decode.map (\_ -> [])
-}


name : Decoder (List String)
name =
    sizedIndex Decode.string


string : Decoder (List String)
string =
    sizedIndex Decode.string
        |> Decode.map (\_ -> [])


localSubRoutines : Decoder (List (List Segment))
localSubRoutines =
    sizedIndex Charstring.decodeAsParts


charstring : Decoder (List Charstring)
charstring =
    sizedIndex (\_ -> Charstring.decode)


charstringWithOptions : { global : Array (List Segment), local : Maybe (Array (List Segment)) } -> Decoder (List Charstring)
charstringWithOptions subroutines =
    sizedIndex (\size -> Charstring.decodeWithOptions size subroutines)


globalSubRoutines : Decoder (List (List Segment))
globalSubRoutines =
    sizedIndex Charstring.decodeAsParts



{-
      card16
          |> Decode.andThen
              (\count ->
                  if Debug.log "global subroutines count" count == 0 then
                      Decode.succeed []

                  else
                      offSize
                          |> Decode.andThen
                              (\offsetSize ->
                                  exactly (count + 1) (offset offsetSize)
                                      |> Decode.andThen
                                          (\offsets ->
                                              let
                                                  _ =
                                                      Debug.log "offsets" offsets

                                                  deltas =
                                                      List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                              in
                                              Decode.loop ( [], deltas ) globalSubroutinesHelp
                                          )
                              )
              )


   globalSubroutinesHelp : ( List Charstring, List Int ) -> Decoder (Step ( List Charstring, List Int ) (List Charstring))
   globalSubroutinesHelp ( accum, remaining ) =
       case remaining of
           first :: rest ->
               Decode.map (\new -> Loop ( new :: accum, rest )) (Charstring.decodeWithOptions { global = List.reverse accum, local = Nothing })

           [] ->
               Decode.succeed (Done (List.reverse accum))

-}
-- Helpers


sizedIndex : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndex elementDecoder =
    card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                            in
                                            mapM elementDecoder deltas
                                        )
                            )
            )


sizedIndexDebug : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndexDebug elementDecoder =
    card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                            in
                                            mapM
                                                (\( n, d ) ->
                                                    let
                                                        _ =
                                                            Debug.log "iteration" n
                                                    in
                                                    elementDecoder d
                                                )
                                                (List.indexedMap (\i k -> ( i, k )) deltas)
                                        )
                            )
            )


card8 : Decoder Int
card8 =
    Decode.unsignedInt8


card16 : Decoder Int
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
                |> Decode.map (Debug.log "---------------> unchecked logic")

        Bytes4 ->
            Decode.unsignedInt32 BE


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


exactlyDebug : Int -> Decoder a -> Decoder (List a)
exactlyDebug len decoder =
    let
        looper ( n, xs ) =
            let
                _ =
                    Debug.log "iteration" n
            in
            if n > len then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n + 1, x :: xs )) decoder
    in
    Decode.loop ( len, [] ) looper


exactly : Int -> Decoder a -> Decoder (List a)
exactly len decoder =
    Decode.loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Done (List.reverse xs))

    else
        Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
