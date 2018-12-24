module Decode exposing (Decoder, decode, unsignedInt8)

import Bytes exposing (Bytes)
import Bytes.Decode as Internal


type Decoder a
    = Decoder ({ size : Int, decoder : Internal.Decoder a } -> { size : Int, decoder : Internal.Decoder a })


unsignedInt8 : Decoder Int
unsignedInt8 =
    Decoder <| \s -> { size = s.size + 1, decoder = Internal.unsignedInt8 }


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder a) (Decoder b) =
    Decoder <|
        \state ->
            let
                afterA =
                    a state

                afterB =
                    b afterA
            in
            { size = afterB.size
            , decoder = Internal.map2 f afterA.decoder afterB.decoder
            }


getOffset : Decoder Int
getOffset =
    \s ->
        { size = s.size, decoder = Internal.succeed s.size }


decode : Decoder a -> Bytes -> Maybe a
decode (Decoder { decoder }) =
    Internal.decode decoder
