module Decode.Opentype exposing (LongDateTime, Tag(..), Version, fixed, fword, int16, longDateTime, offset16, offset32, tag, ufword, uint16, uint32, version, tagAsString)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Decode.Extra exposing (andMap)


type Tag
    = Tag { tag : String, fragment1 : Int, fragment2 : Int, fragment3 : Int, fragment4 : Int }

tagAsString : Decode.Decoder String 
tagAsString= 
    tag
        |> Decode.map (\(Tag r ) -> r.tag)

tag =
    let
        helper f1 f2 f3 f4 =
            Tag
                { tag =
                    String.fromList
                        [ Char.fromCode f1
                        , Char.fromCode f2
                        , Char.fromCode f3
                        , Char.fromCode f4
                        ]
                , fragment1 = f1
                , fragment2 = f2
                , fragment3 = f3
                , fragment4 = f4
                }
    in
    Decode.succeed helper
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8


int16 =
    Decode.signedInt16 BE


uint16 =
    Decode.unsignedInt16 BE


uint32 =
    Decode.unsignedInt32 BE


offset16 =
    Decode.unsignedInt16 BE


offset32 =
    Decode.unsignedInt32 BE


fixed =
    Decode.signedInt32 BE


fword =
    Decode.signedInt16 BE


ufword =
    Decode.unsignedInt16 BE


type alias Version =
    { major : Int, minor : Int }


version =
    Decode.succeed Version
        |> andMap uint16
        |> andMap uint16


{-| Date represented in number of seconds since 12:00 midnight, January 1, 1904
The value is represented as a signed 64-bit integer.

Sadly, JS doesn't have 64-bit integers (only 51 (52 if including the sign) bits can be used reliably)
So we just store the raw bits

-}
type LongDateTime
    = LongDateTime Int Int


longDateTime =
    Decode.map2 LongDateTime uint32 uint32
