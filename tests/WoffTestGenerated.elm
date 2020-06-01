module WoffTestGenerated exposing (tests)

import Bytes
import Bytes.Encode as Encode 
import Bytes.Decode as Decode 
import Woff


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Generated.Vibur as Vibur
import Generated.FiraSansMedium as FiraSansMedium

pipeline name data =
    test name <|
        \_ ->
            case Woff.decode data of
                Nothing ->
                    Expect.fail "decoder failed"

                Just v ->
                    Expect.pass


tests = 
    describe "generated woff2 tests" 
        [ pipeline "Vibur" Vibur.bytes, pipeline "FiraSansMedium" FiraSansMedium.bytes  ]
