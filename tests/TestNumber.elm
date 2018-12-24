module TestNumber exposing (suite)

import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Number
import Test exposing (..)


encode =
    Encode.encode << Encode.sequence << List.map Encode.unsignedInt8


suite : Test
suite =
    describe "integer & real"
        [ integer
        , real
        ]


integerHelp : Int -> List Int -> Test
integerHelp expected tape =
    test (String.fromInt expected) <|
        \_ ->
            Decode.decode Number.number (encode tape)
                |> Expect.equal (Just (toFloat expected))


integer =
    describe "integer"
        [ integerHelp 0 [ 0x8B ]
        , integerHelp 100 [ 0xEF ]
        , integerHelp -100 [ 0x27 ]
        , integerHelp 1000 [ 0xFA, 0x7C ]
        , integerHelp -1000 [ 0xFE, 0x7C ]
        , integerHelp 10000 [ 0x1C, 0x27, 0x10 ]
        , integerHelp -10000 [ 0x1C, 0xD8, 0xF0 ]
        , integerHelp 100000 [ 0x1D, 0x00, 0x01, 0x86, 0xA0 ]
        , integerHelp -100000 [ 0x1D, 0xFF, 0xFE, 0x79, 0x60 ]
        ]


real =
    describe "real"
        [ test "-2.25" <|
            \_ ->
                let
                    tape =
                        encode [ 0x1E, 0xE2, 0xA2, 0x5F, 0x0F ]
                in
                Decode.decode Number.number tape
                    |> Expect.equal (Just -2.25)
        , test "with exponent" <|
            \_ ->
                let
                    tape =
                        encode [ 0x1E, 0x0A, 0x14, 0x05, 0x41, 0xC3, 0xFF, 0x0F ]
                in
                case Decode.decode Number.number tape of
                    Just v ->
                        Expect.within (Absolute 1.0e-14) 1.40541e-4 v

                    Nothing ->
                        Expect.fail "number parse was Nothing"
        ]
