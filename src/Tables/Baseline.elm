module Tables.Baseline exposing (Baseline, decode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (Tag, int16, offset16, offset32, tag, uint16)


x =
    5
