import glob

import glob
import os

test_file_format = """
module Generated.{} exposing(raw, bytes) 

import Bytes.Encode as Encode

bytes = 
    raw 
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode

raw : List Int 
raw = {}
"""

gutenberg_format = """module {moduleName}TestGenerated exposing (tests)

import Bytes
import Bytes.Encode as Encode 
import Bytes.Decode as Decode 
import {moduleName}


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

{imports}

pipeline name data =
    test name <|
        \_ ->
            case {moduleName}.decode data of
                Nothing ->
                    Expect.fail "decoder failed"

                Just v ->
                    Expect.pass


tests = 
    describe "generated woff2 tests" 
        [ {tests}  ]
"""

def generate(extension, moduleName):
    bases = [] 
    paths = glob.glob(f"sources/*.{extension}")

    for sourcePath in paths:
        bases.append(os.path.splitext(os.path.basename(sourcePath))[0])

    for sourcePath in paths: 
        with open(sourcePath, 'rb') as source:
            bs = source.read() 
            result = [ v for v in bs ] 

            base = os.path.basename(sourcePath)
            destName = os.path.splitext(base)[0]

            with open(destName + ".elm", 'w') as dst: 
                dst.write(test_file_format.format(destName, result))


    imports = "\n".join("import Generated." + name + " as " + name for name in bases)

    tests = ", ".join("pipeline \"" + name + "\" " + name + ".bytes" for name in bases) 

    with open(f"../{moduleName}TestGenerated.elm", 'w') as dst:
        dst.write(gutenberg_format.format(moduleName=moduleName, imports=imports, tests=tests))



if __name__ == '__main__':
    generate("woff2", "Woff2")
    generate("woff", "Woff")
    # generate("ttf", "TrueType")
    generate("otf", "OpenType")
    generate("ttf", "OpenType")

