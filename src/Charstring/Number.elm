module Charstring.Number exposing (Number(..), decode, decodeHelp)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)


type Number
    = Fixed Int
    | Integer Int


decode : Decoder Number
decode =
    Decode.andThen decodeHelp Decode.unsignedInt8


decodeHelp : Int -> Decoder Number
decodeHelp byte =
    case byte of
        28 ->
            Decode.signedInt16 BE
                |> Decode.map Integer

        255 ->
            Decode.unsignedInt32 BE
                |> Decode.map Fixed

        _ ->
            if byte >= 32 && byte <= 246 then
                Decode.succeed (Integer (byte - 139))

            else if byte >= 247 && byte <= 250 then
                Decode.unsignedInt8
                    |> Decode.andThen
                        (\low ->
                            let
                                high =
                                    (byte - 247) * 256
                            in
                            Decode.succeed (Integer (high + low + 108))
                        )

            else if byte >= 251 && byte <= 254 then
                Decode.unsignedInt8
                    |> Decode.andThen
                        (\low ->
                            let
                                high =
                                    (byte - 251) * 256
                            in
                            Decode.succeed
                                (Integer (-high - low - 108))
                        )

            else
                -- unreachable, all of u8 is covered
                Decode.fail
