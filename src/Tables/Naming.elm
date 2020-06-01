module Tables.Naming exposing (Naming, decode)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra exposing (andMap)



-- Naming Table


type alias NameRecord =
    { platformID : Int
    , encodingID : Int
    , languageID : Int
    , nameID : Int
    , length : Int
    , offset : Int
    }


nameRecordDecoder : Decoder NameRecord
nameRecordDecoder =
    Decode.succeed NameRecord
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)


type alias LangTagRecord =
    { length : Int, offset : Int }


langTagRecordDecoder : Decoder LangTagRecord
langTagRecordDecoder =
    Decode.succeed LangTagRecord
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)


type Naming
    = NameFormat0 { count : Int, stringOffset : Int, nameRecords : List NameRecord, stringData : String }
    | NameFormat1 { count : Int, stringOffset : Int, nameRecords : List NameRecord, langTags : List LangTagRecord, stringData : String }
    | NameFormatError


decode =
    Decode.map2 Tuple.pair uint16 uint16
        |> Decode.andThen
            (\( version, count ) ->
                case Debug.log "version" version of
                    0 ->
                        -- Decode.succeed (\stringOffset nameRecord stringData -> NameFormat0 { count = count, stringOffset = stringOffset, nameRecord = nameRecord, stringData = stringData })
                        Decode.succeed (\stringOffset nameRecords -> NameFormat0 { count = count, stringOffset = stringOffset, nameRecords = nameRecords, stringData = "" })
                            |> andMap offset16
                            |> andMap (Decode.Extra.exactly 1 nameRecordDecoder)

                    {-
                       |> andMap (exactly count nameRecordDecoder)
                       |> andMap (Decode.succeed "")
                    -}
                    1 ->
                        {-
                           let
                               langTag =
                                   uint16 |> Decode.andThen (\n -> exactly n langTagRecordDecoder)
                           in
                           Decode.succeed
                               (\stringOffset nameRecord langTags stringData ->
                                   NameFormat1 { count = count, stringOffset = stringOffset, nameRecord = nameRecord, langTags = langTags, stringData = stringData }
                               )
                               |> andMap uint16
                               |> andMap (exactly count nameRecordDecoder)
                               |> andMap langTag
                               |> andMap (Decode.succeed "")
                        -}
                        Debug.log "fail in naming table decoder 1" Decode.fail

                    _ ->
                        Debug.log ("fail in naming table decoder " ++ String.fromInt version) (Decode.succeed NameFormatError)
            )



-- helpers


int16 =
    Decode.signedInt16 BE


uint16 =
    Decode.unsignedInt16 BE


offset16 =
    Decode.unsignedInt16 BE
