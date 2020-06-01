module TrueTypeTestGenerated exposing (tests)

import Bytes
import Bytes.Encode as Encode 
import Bytes.Decode as Decode 
import TrueType


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Generated.ChangaRegular as ChangaRegular

pipeline name data =
    test name <|
        \_ ->
            case TrueType.decode data of
                Nothing ->
                    Expect.fail "decoder failed"

                Just v ->
                    Expect.pass


tests = 
    describe "generated woff2 tests" 
        [ pipeline "ChangaRegular" ChangaRegular.bytes  ]
