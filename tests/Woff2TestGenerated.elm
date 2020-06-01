module Woff2TestGenerated exposing (tests)

import Bytes
import Bytes.Encode as Encode 
import Bytes.Decode as Decode 
import Woff2


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Generated.NotoSansItalic as NotoSansItalic
import Generated.NotoSansBoldItalic as NotoSansBoldItalic
import Generated.SourceSansPro as SourceSansPro
import Generated.DMSerif as DMSerif

pipeline name data =
    test name <|
        \_ ->
            case Woff2.decode data of
                Nothing ->
                    Expect.fail "decoder failed"

                Just v ->
                    Expect.pass


tests = 
    describe "generated woff2 tests" 
        [ pipeline "NotoSansItalic" NotoSansItalic.bytes, pipeline "NotoSansBoldItalic" NotoSansBoldItalic.bytes, pipeline "SourceSansPro" SourceSansPro.bytes, pipeline "DMSerif" DMSerif.bytes  ]
