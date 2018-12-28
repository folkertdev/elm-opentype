module Main exposing (main)

import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import CompactFontFormat
import Glyphs exposing (Glyphs)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)
import Http
import Path
import Render
import SubPath
import Svg
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, transform, width)


elmInjectionSize =
    641


type alias Model =
    { count : Int, font : Maybe Font, cff : Maybe CompactFontFormat.Cff, glyphs : Maybe Glyphs }


openSans : Cmd Msg
openSans =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept-Ranges" "bytes" ]
        , url = "../SourceSansPro-It.otf"

        --, url = "../MontserratAlternates-Black.otf"
        --, url = "../Raleway-v4020-Regular.otf"
        , body = Http.emptyBody
        , expect = Http.expectBytesResponse OpenSans keepBytes
        , timeout = Nothing
        , tracker = Nothing
        }


keepBytes : Http.Response Bytes -> Result String Bytes
keepBytes response =
    case response of
        Http.GoodStatus_ _ bytes ->
            Ok bytes

        _ ->
            Err (Debug.toString response)


initialModel : Model
initialModel =
    { count = 0, font = Nothing, cff = Nothing, glyphs = Nothing }


type alias Font =
    { header : OffsetTable
    , records : List TableRecord
    , maximumProfile : MaximumProfile
    , horizontalMetrics : HorizontalMetrics
    , horizontalHeader : HorizontalHeader
    , os2 : OS2
    }


type Msg
    = Increment
    | Decrement
    | OpenSans (Result String Bytes)


type alias OffsetTable =
    { sfntVersion : Tag
    , numTables : Int
    , searchRange : Int
    , entrySelector : Int
    , rangeShift : Int
    }


offsetTableDecoder =
    Decode.succeed OffsetTable
        |> andMap tagDecoder
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE)


type alias TableRecord =
    { tag : Tag
    , checkSum : Int
    , offset : Int
    , length : Int
    }


tableRecordDecoder =
    Decode.succeed TableRecord
        |> andMap tagDecoder
        |> andMap (Decode.unsignedInt32 BE)
        |> andMap (Decode.unsignedInt32 BE)
        |> andMap (Decode.unsignedInt32 BE)


type Tag
    = Tag { tag : String, fragment1 : Int, fragment2 : Int, fragment3 : Int, fragment4 : Int }


tagDecoder =
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


andMap first later =
    Decode.map2 (<|) later first


openFontDecoder =
    (Decode.succeed (\_ a -> a)
        |> andMap (Decode.string 641)
        |> andMap offsetTableDecoder
    )
        |> Decode.andThen
            (\header ->
                Decode.succeed
                    (\records maximumProfile horizontalMetrics horizontalHeader os2 ->
                        { header = header
                        , records = records
                        , maximumProfile = maximumProfile
                        , horizontalMetrics = horizontalMetrics
                        , horizontalHeader = horizontalHeader
                        , os2 = os2
                        }
                    )
                    |> andMap (tableRecordsDecoder header.numTables)
                    |> andMap maxpDecoder
                    |> andMap horizontalMetricsDecoder
                    |> andMap horizontalHeaderDecoder
                    |> andMap os2Decoder
            )


tableRecordsDecoder : Int -> Decoder (List TableRecord)
tableRecordsDecoder tableCount =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n - 1, x :: xs )) tableRecordDecoder
    in
    Decode.loop ( tableCount, [] ) helper


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        OpenSans (Err e) ->
            let
                _ =
                    Debug.log "error in decoding bytes" e
            in
            ( model, Cmd.none )

        OpenSans (Ok bytes) ->
            let
                targetedDecoder =
                    (Decode.succeed (\_ a -> a)
                        |> andMap (Decode.string 631)
                        |> andMap offsetTableDecoder
                    )
                        |> Decode.andThen
                            (\header ->
                                Decode.succeed
                                    (\records ->
                                        { header = header
                                        , records = records
                                        }
                                    )
                                    |> andMap (tableRecordsDecoder header.numTables)
                            )

                file =
                    Decode.decode openFontDecoder bytes

                t =
                    Decode.decode targetedDecoder bytes

                montserratOffset =
                    11512 + elmInjectionSize

                sourceSansProOffset =
                    631 + 6284

                cffDecoder =
                    Decode.map2 (\_ k -> k)
                        (Decode.string <| sourceSansProOffset)
                        CompactFontFormat.decode

                cff =
                    Decode.decode cffDecoder bytes

                glyphs =
                    case cff of
                        Just w ->
                            CompactFontFormat.glyphs sourceSansProOffset bytes w 0

                        Nothing ->
                            let
                                _ =
                                    Debug.log "cff parsing failed, so no glyphs" ()
                            in
                            Nothing

                {-
                             dec =
                                 Decode.map2 (\_ k -> k)
                                     (Decode.string <| elmInjectionSize + 11512 + 26649)
                                     CompactFontFormat.charstring

                             nGlyphs =
                                 Decode.decode dec bytes
                                     |> Debug.log "***************************************"
                   in
                   ( { model | font = file, cff = cff }, Cmd.none )

                -}
            in
            ( { model | font = file, cff = cff, glyphs = glyphs }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "yay"
    , body =
        [ div [] <|
            case model.font of
                Nothing ->
                    []

                Just font ->
                    let
                        row a b =
                            tr [] [ td [] [ text a ], td [] [ b ] ]

                        rec r =
                            row (Debug.toString r.tag) (text (Debug.toString r.offset))
                    in
                    [ table []
                        [ row "header" (text <| Debug.toString font.header)
                        , row "maximumProfile" (text <| Debug.toString font.maximumProfile)
                        , row "horizontalMetrics" (text <| Debug.toString font.horizontalMetrics)
                        , row "horizontalHeader" (text <| Debug.toString font.horizontalHeader)
                        , row "os2" (text <| Debug.toString font.os2)
                        , text "records"
                        ]
                    , table [] (List.map rec <| List.sortBy .offset font.records)
                    ]
        , div [] <|
            case model.glyphs of
                Nothing ->
                    [ text "NOPE no glyphs" ]

                Just glyphs ->
                    -- a = 34
                    let
                        f =
                            33

                        e =
                            32

                        l =
                            39

                        m =
                            40
                    in
                    [ Svg.svg [ width "2000", height "1000" ]
                        [ renderGlyph f glyphs 0
                        , renderGlyph l glyphs 500
                        , renderGlyph m glyphs 800
                        ]
                    ]
        ]
    }


renderGlyph index glyphs offset =
    case Glyphs.charstring index glyphs of
        Nothing ->
            text ""

        Just subroutine ->
            let
                _ =
                    Debug.log "subroutine" subroutine

                subpaths =
                    Render.convert subroutine
                        |> Debug.log "subpaths"
            in
            Path.element subpaths [ fill "none", stroke "black", strokeWidth "2", transform ("scale(1, -1)  translate(" ++ String.fromInt offset ++ ", -750)") ]



--M205,69 C309,98 251,69 205,69 L355,154 C336,434 368,413 398,362 C118,314 205,434 299,434 C151,69 118,108 118,187
{-

   x =
       [ MoveTo { x = 205, y = 69 }
       , Many [ CurveTo { x = 151, y = 69 } { x = 118, y = 108 } { x = 118, y = 187 }, CurveTo { x = 118, y = 314 } { x = 205, y = 434 } { x = 299, y = 434 }, CurveTo { x = 336, y = 434 } { x = 368, y = 413 } { x = 398, y = 362 } ]
       , LineTo { x = 355, y = 154 }
       , CurveTo { x = 309, y = 98 } { x = 251, y = 69 } { x = 205, y = 69 }
       ]
-}


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, openSans )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



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


type NamingTable
    = NameFormat0 { count : Int, stringOffset : Int, nameRecords : List NameRecord, stringData : String }
    | NameFormat1 { count : Int, stringOffset : Int, nameRecords : List NameRecord, langTags : List LangTagRecord, stringData : String }


namingTableDecoder =
    Decode.map2 Tuple.pair uint16 uint16
        |> Decode.andThen
            (\( version, count ) ->
                case Debug.log "version" version of
                    0 ->
                        -- Decode.succeed (\stringOffset nameRecord stringData -> NameFormat0 { count = count, stringOffset = stringOffset, nameRecord = nameRecord, stringData = stringData })
                        Decode.succeed (\stringOffset nameRecords -> NameFormat0 { count = count, stringOffset = stringOffset, nameRecords = nameRecords, stringData = "" })
                            |> andMap offset16
                            |> andMap (exactly 1 nameRecordDecoder)

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
                        Debug.log "fail in naming table decoder" Decode.fail

                    _ ->
                        Debug.log "fail in naming table decoder" Decode.fail
            )



-- Maxp


type MaximumProfile
    = V0_5 MaximumProfileV0_5
    | V1_0 MaximumProfileV1_0


type alias MaximumProfileV0_5 =
    { numGlyphs : Int
    }


type alias MaximumProfileV1_0 =
    { numGlyphs : Int
    , maxPoints : Int
    , maxContours : Int
    , maxCompositePoints : Int
    , maxCompositeContours : Int
    , maxZones : Int
    , maxTwilightPoints : Int
    , maxStorage : Int
    , maxFunctionDefs : Int
    , maxInstructionDefs : Int
    , maxStackElements : Int
    , maxSizeOfInstructions : Int
    , maxComponentElements : Int
    , maxComponentDepth : Int
    }


maxpDecoder =
    let
        version0_5 =
            Decode.succeed MaximumProfileV0_5
                |> andMap uint16

        version1_0 =
            Decode.succeed MaximumProfileV1_0
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
                |> andMap uint16
    in
    fixed
        |> Decode.andThen
            (\version ->
                case version of
                    0x00010000 ->
                        Decode.map V1_0 version1_0

                    0x5000 ->
                        Decode.map V0_5 version0_5

                    _ ->
                        Debug.log "fail in maxp" Decode.fail
            )


type alias HorizontalMetrics =
    ()


horizontalMetricsDecoder : Decoder HorizontalMetrics
horizontalMetricsDecoder =
    Decode.map (\_ -> ()) (Decode.string 5928)



-- HorizontalHeader


type alias HorizontalHeader =
    { majorVersion : Int
    , minorVersion : Int
    , ascender : Int
    , descender : Int
    , lineGap : Int
    , advanceWidthMax : Int
    , minLeftSideBearing : Int
    , minRightSideBearing : Int
    , xMaxExtent : Int
    , caretSlopeRise : Int
    , caretSlopeRun : Int
    , caretOffset : Int
    , reserved1 : Int
    , reserved2 : Int
    , reserved3 : Int
    , reserved4 : Int
    , metricDataFormat : Int
    , numberOfHMetrics : Int
    }


horizontalHeaderDecoder : Decoder HorizontalHeader
horizontalHeaderDecoder =
    Decode.succeed HorizontalHeader
        |> andMap uint16
        |> andMap uint16
        |> andMap fword
        |> andMap fword
        |> andMap fword
        |> andMap ufword
        |> andMap fword
        |> andMap fword
        |> andMap fword
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap int16
        |> andMap uint16



-- OS/2


type alias OS2 =
    { version : Int }


os2Decoder : Decoder OS2
os2Decoder =
    uint16
        |> Decode.andThen
            (\version ->
                case version of
                    0x05 ->
                        Decode.succeed (OS2 5)

                    0x04 ->
                        Decode.succeed (OS2 4)

                    0x03 ->
                        Decode.succeed (OS2 3)

                    0x02 ->
                        Decode.succeed (OS2 2)

                    0x01 ->
                        Decode.succeed (OS2 1)

                    0x00 ->
                        Decode.succeed (OS2 0)

                    _ ->
                        Decode.succeed (OS2 version)
            )



-- helpers


int16 =
    Decode.signedInt16 BE


uint16 =
    Decode.unsignedInt16 BE


offset16 =
    Decode.unsignedInt16 BE


fixed =
    Decode.signedInt32 BE


fword =
    Decode.signedInt32 BE


ufword =
    Decode.unsignedInt32 BE


exactly : Int -> Decoder a -> Decoder (List a)
exactly tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
    in
    Decode.loop ( tableCount, [] ) helper


keep =
    andMap


drop : Decoder drop -> Decoder keep -> Decoder keep
drop toDrop toKeep =
    Decode.map2 (\k _ -> k) toKeep toDrop
