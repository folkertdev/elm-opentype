module Tables.OS2 exposing (OS2, decode)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (Tag, tag)


type OS2
    = Version_0 Version0
    | Other Int


decode : Decoder OS2
decode =
    uint16
        |> Decode.andThen
            (\version ->
                case version of
                    0x05 ->
                        Decode.succeed (Other 5)

                    0x04 ->
                        Decode.succeed (Other 4)

                    0x03 ->
                        Decode.succeed (Other 3)

                    0x02 ->
                        Decode.succeed (Other 2)

                    0x01 ->
                        Decode.succeed (Other 1)

                    0x00 ->
                        version0 0
                            |> Decode.map Version_0

                    _ ->
                        Decode.succeed (Other version)
            )


type alias Version0 =
    { version : Int
    , xAvgCharWidth : Int
    , usWeightClass : Int
    , usWidthClass : Int
    , fsType : Int
    , ySubscriptXSize : Int
    , ySubscriptYSize : Int
    , ySubscriptXOffset : Int
    , ySubscriptYOffset : Int
    , ySuperscriptXSize : Int
    , ySuperscriptYSize : Int
    , ySuperscriptXOffset : Int
    , ySuperscriptYOffset : Int
    , yStrikeoutSize : Int
    , yStrikeoutPosition : Int
    , sFamilyClass : Int
    , panose : Array Int
    , ulUnicodeRange1 : Int
    , ulUnicodeRange2 : Int
    , ulUnicodeRange3 : Int
    , ulUnicodeRange4 : Int
    , achVendID : Tag
    , fsSelection : Int
    , usFirstCharIndex : Int
    , usLastCharIndex : Int
    , sTypoAscender : Int
    , sTypoDescender : Int
    , sTypoLineGap : Int
    , usWinAscent : Int
    , usWinDescent : Int
    }


version0 version =
    Decode.succeed (Version0 version)
        |> andMap int16
        |> andMap uint16
        |> andMap uint16
        |> andMap uint16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap (Decode.Extra.array 10 Decode.unsignedInt8)
        |> andMap uint32
        |> andMap uint32
        |> andMap uint32
        |> andMap uint32
        |> andMap tag
        |> andMap uint16
        |> andMap uint16
        |> andMap uint16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap uint16
        |> andMap uint16


int16 =
    Decode.signedInt16 BE


uint16 =
    Decode.unsignedInt16 BE


uint32 =
    Decode.unsignedInt32 BE


offset16 =
    Decode.unsignedInt16 BE


fixed =
    Decode.signedInt32 BE
