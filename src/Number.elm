module Number exposing (Number(..), Size(..), number, real)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))


type Number
    = Integer Size Int
    | Real Size Float


type Size
    = Size Int


number : Decoder Number
number =
    Decode.unsignedInt8
        |> Decode.andThen
            (\first ->
                if first >= 0x20 && first <= 0xF6 then
                    Decode.succeed (Integer (Size 1) (first - 139))

                else if first >= 0xF7 && first <= 0xFA then
                    Decode.unsignedInt8
                        |> Decode.andThen
                            (\second ->
                                Decode.succeed (Integer (Size 2) ((first - 247) * 256 + second + 108))
                            )

                else if first >= 0xFB && first <= 0xFE then
                    Decode.unsignedInt8
                        |> Decode.andThen
                            (\second ->
                                Decode.succeed (Integer (Size 2) (-(first - 251) * 256 - second - 108))
                            )

                else
                    case first of
                        0x1C ->
                            Decode.signedInt16 BE
                                |> Decode.map (Integer (Size 3))

                        0x1D ->
                            Decode.signedInt32 BE
                                |> Decode.map (Integer (Size 4))

                        0x1E ->
                            Decode.map (\( s, v ) -> Real s v) real

                        _ ->
                            let
                                _ =
                                    Debug.log "first fell through" first
                            in
                            Decode.fail
            )


real : Decoder ( Size, Float )
real =
    Decode.loop ( "", Nothing, 0 ) parseRealHelp
        |> Decode.andThen
            (\( size, str ) ->
                case String.toFloat str of
                    Just v ->
                        Decode.succeed ( Size size, v )

                    Nothing ->
                        Decode.fail
            )


parseRealHelp ( buffer, high, size ) =
    case high of
        Nothing ->
            -- parse new byte
            Decode.unsignedInt8
                |> Decode.andThen
                    (\byte ->
                        case matchNibble (Bitwise.shiftRightBy 4 byte) of
                            Ok chars ->
                                Decode.succeed (Loop ( buffer ++ chars, Just (Bitwise.and byte 0x0F), size + 1 ))

                            Err True ->
                                Decode.succeed (Done ( size, buffer ))

                            Err False ->
                                Decode.fail
                    )

        Just nibble ->
            case matchNibble nibble of
                Ok chars ->
                    Decode.succeed (Loop ( buffer ++ chars, Nothing, size ))

                Err True ->
                    Decode.succeed (Done ( size, buffer ))

                Err False ->
                    Decode.fail


matchNibble : Int -> Result Bool String
matchNibble nibble =
    if nibble >= 0 && nibble <= 9 then
        (Char.toCode '0' + nibble)
            |> Char.fromCode
            |> String.fromChar
            |> Ok

    else
        case nibble of
            0x0A ->
                Ok "."

            0x0B ->
                Ok "e"

            0x0C ->
                Ok "e-"

            0x0E ->
                Ok "-"

            0x0F ->
                Err True

            _ ->
                Err False
