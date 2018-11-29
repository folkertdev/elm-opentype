module Operator exposing (DICT, Operand(..), decode)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Number exposing (Size(..))
import Utils exposing (exactly, until)


type alias DICT =
    -- Dict String (Maybe Operand)
    List ( String, Operand )


sid =
    Decode.unsignedInt16 BE
        |> Decode.map SID


number =
    Number.number
        |> Decode.map Number


array : Int -> Decoder Operand
array numberOfBytes =
    Decode.loop ( numberOfBytes, [] ) arrayHelp


arrayHelp ( numberOfBytes, accum ) =
    if numberOfBytes <= 0 then
        Decode.succeed (Done (Array (List.reverse accum)))

    else
        Number.number
            |> Decode.andThen
                (\v ->
                    case v of
                        Number.Integer (Size size) _ ->
                            Decode.succeed (Loop ( numberOfBytes - size, v :: accum ))

                        Number.Real (Size size) _ ->
                            Decode.succeed (Loop ( numberOfBytes - size, v :: accum ))
                )


boolean =
    Number.number
        |> Decode.andThen
            (\v ->
                case v of
                    Number.Integer _ 0 ->
                        Decode.succeed (Boolean False)

                    Number.Integer _ _ ->
                        Decode.succeed (Boolean True)

                    _ ->
                        Decode.fail
            )


type Operand
    = Number Number.Number
    | Boolean Bool
    | SID Int
    | Array (List Number.Number)
    | Delta (List Number.Number)
    | Private Number.Number Number.Number


decode : Decoder DICT
decode =
    exactly 8 (Decode.loop ( Nothing, [] ) decodeHelp)



{-
   decodeHelp accum =
       Decode.unsignedInt8
           |> Decode.andThen
               (\firstByte ->
                   if firstByte /= 6 && firstByte /= 19 && firstByte >= 0 && firstByte <= 21 then
                       -- found an operator
                       let
                           bytes =
                               Encode.encode <| Encode.sequence (List.map Encode.unsignedInt8 (List.reverse accum))
                       in
                       if firstByte == 12 then
                           Decode.unsignedInt8
                               |> Decode.andThen (\secondByte -> decodeDoubleByteOperator secondByte bytes)
                               |> Decode.map Done

                       else
                           decodeSingleByteOperator firstByte bytes
                               |> Decode.map Done

                   else
                       Decode.succeed (Loop (firstByte :: accum))
               )
-}


encode accum =
    Encode.encode <| Encode.sequence (List.map Encode.unsignedInt8 accum)


decodeHelp ( readByte, accum ) =
    (case readByte of
        Nothing ->
            Decode.unsignedInt8

        Just b ->
            Decode.succeed b
    )
        |> Decode.andThen
            (\firstByte ->
                case firstByte of
                    28 ->
                        Decode.map2
                            (\b1 b2 ->
                                let
                                    _ =
                                        Debug.log "(b1, b2)" ( firstByte, b1, b2 )
                                in
                                Loop ( Nothing, b2 :: b1 :: firstByte :: accum )
                            )
                            Decode.unsignedInt8
                            Decode.unsignedInt8

                    29 ->
                        Decode.map4 (\b1 b2 b3 b4 -> Loop ( Nothing, b4 :: b3 :: b2 :: b1 :: firstByte :: accum ))
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8

                    30 ->
                        until (\b -> Bitwise.and b 0x0F /= 0x0F) Decode.unsignedInt8
                            |> Decode.andThen
                                (\( nextByte, realBytes ) ->
                                    if nextByte < 22 then
                                        let
                                            bytes =
                                                encode (List.reverse <| List.reverse realBytes ++ accum)
                                        in
                                        if firstByte == 12 then
                                            Decode.unsignedInt8
                                                |> Decode.andThen (\secondByte -> decodeDoubleByteOperator secondByte bytes)
                                                |> Decode.map Done

                                        else
                                            decodeSingleByteOperator firstByte bytes
                                                |> Decode.map Done

                                    else
                                        Decode.succeed (Loop ( Just nextByte, List.reverse realBytes ++ (firstByte :: accum) ))
                                )

                    b ->
                        if b >= 32 && b <= 246 then
                            let
                                _ =
                                    Debug.log "(b )" b
                            in
                            Decode.succeed (Loop ( Nothing, b :: accum ))

                        else if b >= 247 && b <= 254 then
                            Decode.map
                                (\b1 ->
                                    let
                                        _ =
                                            Debug.log "(b1 )" ( firstByte, b1 )
                                    in
                                    Loop ( Nothing, b1 :: firstByte :: accum )
                                )
                                Decode.unsignedInt8

                        else if b < 22 then
                            let
                                bytes =
                                    encode (List.reverse accum)
                            in
                            if firstByte == 12 then
                                Decode.unsignedInt8
                                    |> Decode.andThen (\secondByte -> decodeDoubleByteOperator secondByte bytes)
                                    |> Decode.map Done

                            else
                                decodeSingleByteOperator firstByte bytes
                                    |> Decode.map Done

                        else
                            let
                                _ =
                                    Debug.log "b fell through" b
                            in
                            Decode.fail
            )


tag : String -> Decoder a -> Decoder ( String, a )
tag t d =
    Decode.map (\dd -> ( t, dd )) d


decodeSingleByteOperator operator operandBytes =
    case Debug.log "operator" operator of
        0 ->
            embed sid operandBytes
                |> tag "version"

        1 ->
            embed sid operandBytes
                |> tag "Notice"

        2 ->
            embed sid operandBytes
                |> tag "FullName"

        4 ->
            embed sid operandBytes
                |> tag "Weight"

        5 ->
            embed (array (Bytes.width operandBytes)) operandBytes
                |> tag "FontBBox"

        15 ->
            embed number operandBytes
                |> tag "charset"

        17 ->
            embed number operandBytes
                |> tag "CharStrings"

        18 ->
            embed (Decode.map2 Private Number.number Number.number) operandBytes
                |> tag "Private"

        n ->
            Debug.todo ("cannot handle first byte of value " ++ String.fromInt n)


embed : Decoder a -> Bytes -> Decoder a
embed decoder bytes =
    case Decode.decode decoder bytes of
        Nothing ->
            Decode.fail

        Just v ->
            Decode.succeed v


decodeDoubleByteOperator operator operandBytes =
    case Debug.log "operator 2" operator of
        n ->
            Debug.todo ("cannot handle first byte of value " ++ String.fromInt n)


decodeOperator : Decoder (Dict String (Maybe Operand))
decodeOperator =
    Decode.unsignedInt8
        |> Decode.map (Debug.log "first byte")
        |> Decode.andThen (decodeOperatorHelp Dict.empty)


decodeOperatorHelp : Dict String (Maybe Operand) -> Int -> Decoder (Dict String (Maybe Operand))
decodeOperatorHelp dict firstByte =
    let
        add k v =
            Decode.map (\vv -> Dict.insert k vv dict) (parseOperand v)

        addArray k =
            Decode.map (\vv -> Dict.insert k (Just <| Array <| vv) dict) (whileParseOperand Number.number)
    in
    case firstByte of
        0 ->
            add "version" sid

        1 ->
            add "Notice" sid

        2 ->
            add "FullName" sid

        3 ->
            add "FamilyName" sid

        4 ->
            add "Weight" sid

        5 ->
            addArray "FontBBox"

        6 ->
            add "version" sid

        7 ->
            add "version" sid

        8 ->
            add "version" sid

        13 ->
            add "UniqueID" number

        12 ->
            Decode.unsignedInt8
                |> Decode.andThen (\secondByte -> decodeSecondByte dict secondByte)

        14 ->
            addArray "XUID"

        15 ->
            add "charset" number

        16 ->
            add "encoding" number

        17 ->
            add "CharStrings" number

        18 ->
            -- TODO does this (need to) parse 2 ints?
            add "Private" number

        n ->
            Debug.todo ("cannot handle first byte of value " ++ String.fromInt n)


decodeSecondByte dict secondByte =
    let
        add k v =
            Decode.map (\vv -> Dict.insert k vv dict) (parseOperand v)

        addArray k =
            Decode.map (\vv -> Dict.insert k (Just <| Array <| vv) dict) (whileParseOperand Number.number)

        addDelta k =
            Decode.map (\vv -> Dict.insert k (Just <| Delta <| vv) dict) (whileParseOperand Number.number)
    in
    case secondByte of
        1 ->
            add "isFixedPitch" boolean

        2 ->
            add "ItalicAngle" number

        3 ->
            add "UnderlinePosition" number

        4 ->
            add "UnderlineThickness" number

        5 ->
            add "PaintType" number

        6 ->
            add "CharstringType" number

        7 ->
            addArray "FontMatrix"

        --
        20 ->
            add "SyntheticBase" number

        21 ->
            add "PostScript" sid

        22 ->
            add "BaseFontName" sid

        23 ->
            addDelta "BaseFontBlend"

        --
        30 ->
            -- TODO actually 3 numbers
            add "ROS" sid

        31 ->
            add "CIDFontVersion" sid

        32 ->
            add "CIDFontRevision" number

        33 ->
            add "CIDFontType" number

        34 ->
            add "CIDCount" number

        35 ->
            add "UIDBase" number

        36 ->
            add "FDArray" number

        37 ->
            add "FDSelect" number

        38 ->
            add "FontName" sid

        n ->
            Debug.todo ("cannot handle second byte of value " ++ String.fromInt n)


peek : Int -> Decoder a -> Decoder a
peek n decoder =
    Decode.bytes 1
        |> Decode.andThen
            (\copy ->
                case Decode.decode decoder copy of
                    Nothing ->
                        Decode.fail

                    Just v ->
                        Decode.succeed v
            )


peekUnsignedInt8 : Decoder Int
peekUnsignedInt8 =
    peek 1 Decode.unsignedInt8


parseOperand : Decoder a -> Decoder (Maybe a)
parseOperand decoder =
    peekUnsignedInt8
        |> Decode.andThen
            (\n ->
                if n == 28 || n == 29 || n == 30 || (n >= 32 && n <= 255) then
                    Decode.map Just decoder

                else
                    Decode.succeed Nothing
            )


whileParseOperand decoder =
    Decode.loop [] (whileParseOperandHelp decoder)


whileParseOperandHelp decoder accum =
    parseOperand decoder
        |> Decode.andThen
            (\maybeDecoded ->
                case maybeDecoded of
                    Just operand ->
                        Decode.succeed (Loop (operand :: accum))

                    Nothing ->
                        Decode.succeed (Done (List.reverse accum))
            )


x operator =
    case operator of
        0x00 ->
            ( "Version"
            , Decode.succeed Nothing
            )

        0x01 ->
            ( "Notice"
            , Decode.succeed Nothing
            )

        0x02 ->
            ( "FullName"
            , Decode.succeed Nothing
            )

        0x03 ->
            ( "FamilyName"
            , Decode.succeed Nothing
            )

        0x04 ->
            ( "Weight"
            , Decode.succeed Nothing
            )

        0x05 ->
            ( "FontBBox"
            , Decode.succeed (Just [ 0, 0, 0, 0 ])
            )

        0x06 ->
            ( "BlueValues"
            , Decode.succeed Nothing
            )

        0x07 ->
            ( "OtherBlues"
            , Decode.succeed Nothing
            )

        0x08 ->
            ( "FamilyBlues"
            , Decode.succeed Nothing
            )

        0x09 ->
            ( "FamilyOtherBlues"
            , Decode.succeed Nothing
            )

        0x0A ->
            ( "StdHW"
            , Decode.succeed Nothing
            )

        0x0B ->
            ( "StdVW"
            , Decode.succeed Nothing
            )

        -- 0x0c -> ("Escape" ,  Decode.succeed  )
        0x0D ->
            ( "UniqueID"
            , Decode.succeed Nothing
            )

        0x0E ->
            ( "XUID"
            , Decode.succeed Nothing
            )

        0x0F ->
            ( "CharSet"
            , Decode.succeed (Just [ 0 ])
            )

        0x10 ->
            ( "Encoding"
            , Decode.succeed (Just [ 0 ])
            )

        0x11 ->
            ( "CharStrings"
            , Decode.succeed Nothing
            )

        0x12 ->
            ( "Private"
            , Decode.succeed Nothing
            )

        0x13 ->
            ( "Subrs"
            , Decode.succeed Nothing
            )

        0x14 ->
            ( "DefaultWidthX"
            , Decode.succeed (Just [ 0 ])
            )

        0x15 ->
            ( "NominalWidthX"
            , Decode.succeed (Just [ 0 ])
            )

        -- 0x16...0x1b -> ("Reserved" ,  Decode.succeed  )
        -- 0x1c -> ("ShortInt" ,  Decode.succeed  )
        -- 0x1d -> ("LongInt" ,  Decode.succeed  )
        -- 0x1e -> ("BCD" ,  Decode.succeed  )
        -- 0x1f -> ("Reserved" ,  Decode.succeed  )
        -- 0x20...0xf6 -> ("<numbers" ,  Decode.succeed > )
        -- 0xf7...0xfe -> ("<numbers" ,  Decode.succeed > )
        -- 0xff -> ("Reserved" ,  Decode.succeed  )
        0x0C00 ->
            ( "Copyright"
            , Decode.succeed Nothing
            )

        0x0C01 ->
            ( "IsFixedPitch"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C02 ->
            ( "ItalicAngle"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C03 ->
            ( "UnderlinePosition"
            , Decode.succeed (Just [ -100 ])
            )

        0x0C04 ->
            ( "UnderlineThickness"
            , Decode.succeed (Just [ 50 ])
            )

        0x0C05 ->
            ( "PaintType"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C06 ->
            ( "CharStringType"
            , Decode.succeed (Just [ 2 ])
            )

        0x0C07 ->
            ( "FontMatrix"
            , Decode.succeed (Just [ 0.001, 0.0, 0.0, 0.001, 0.0, 0.0 ])
            )

        0x0C08 ->
            ( "StrokeWidth"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C09 ->
            ( "BlueScale"
            , Decode.succeed (Just [ 0.039625 ])
            )

        0x0C0A ->
            ( "BlueShift"
            , Decode.succeed (Just [ 7 ])
            )

        0x0C0B ->
            ( "BlueFuzz"
            , Decode.succeed (Just [ 1 ])
            )

        0x0C0C ->
            ( "StemSnapH"
            , Decode.succeed Nothing
            )

        0x0C0D ->
            ( "StemSnapV"
            , Decode.succeed Nothing
            )

        0x0C0E ->
            ( "ForceBold"
            , Decode.succeed (Just [ 0 ])
            )

        -- 0x0c0f...0x0c10 -> ("Reserved" ,  Decode.succeed  )
        0x0C11 ->
            ( "LanguageGroup"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C12 ->
            ( "ExpansionFactor"
            , Decode.succeed (Just [ 0.06 ])
            )

        0x0C13 ->
            ( "InitialRandomSeed"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C14 ->
            ( "SyntheticBase"
            , Decode.succeed Nothing
            )

        0x0C15 ->
            ( "PostScript"
            , Decode.succeed Nothing
            )

        0x0C16 ->
            ( "BaseFontName"
            , Decode.succeed Nothing
            )

        0x0C17 ->
            ( "BaseFontBlend"
            , Decode.succeed Nothing
            )

        -- 0x0c18...0x0c1d -> ("Reserved" ,  Decode.succeed  )
        0x0C1E ->
            ( "ROS"
            , Decode.succeed Nothing
            )

        0x0C1F ->
            ( "CIDFontVersion"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C20 ->
            ( "CIDFontRevision"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C21 ->
            ( "CIDFontType"
            , Decode.succeed (Just [ 0 ])
            )

        0x0C22 ->
            ( "CIDCount"
            , Decode.succeed (Just [ 8720 ])
            )

        0x0C23 ->
            ( "UIDBase"
            , Decode.succeed Nothing
            )

        0x0C24 ->
            ( "FDArray"
            , Decode.succeed Nothing
            )

        0x0C25 ->
            ( "FDSelect"
            , Decode.succeed Nothing
            )

        0x0C26 ->
            ( "FontName"
            , Decode.succeed Nothing
            )

        -- 0x0c27...0x0cff -> ("Reserved" ,  Decode.succeed  )
        _ ->
            Debug.todo "what?"
