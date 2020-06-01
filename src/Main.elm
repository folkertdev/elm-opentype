module Main exposing (main)

import Array exposing (Array)
import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import Char
import Charset
import Charstring exposing (Operation)
import CompactFontFormat
import CompactFontFormat.Svg as Render
import Decode.CompactFontFormat exposing (GID(..), SID(..))
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (Tag(..), tag)
import Dict exposing (Dict)
import Encoding
import Font exposing (Font)
import Glyphs exposing (Glyphs)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes
import Html.Events exposing (onClick)
import Http
import Paragraph
import Path
import SubPath
import Svg exposing (Svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, transform, width)
import Tables.CharacterMapping exposing (CharacterMapping, Encoding(..))
import Tables.Header exposing (BoundingBox)
import Tables.HorizontalHeader
import Tables.HorizontalMetrics
import Tables.MaximumProfile
import Tables.Naming
import Tables.OS2
import Woff2


sourceSansPro =
    { url = "../SourceSansPro-It.otf", offset = 631 }


sourceSansProWoff2 =
    { url = "../sourcesanspro.woff2", offset = 630 }


montserrat =
    { url = "../MontserratAlternates-Black.otf", offset = 641 }


currentFont =
    sourceSansProWoff2


type alias FontData =
    { cff : CompactFontFormat.CompactFont
    , cmap : Tables.CharacterMapping.CharacterMapping
    , advanceWidths : Array Int
    , glyphBoundingBox : BoundingBox
    }


type alias Model =
    { count : Int
    , font : Maybe Font
    , fontData : Maybe FontData
    }


type Msg
    = LoadFont (Result String Bytes)


loadFont : Cmd Msg
loadFont =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept-Ranges" "bytes" ]
        , url = currentFont.url

        --, url = "../Raleway-v4020-Regular.otf"
        , body = Http.emptyBody
        , expect = Http.expectBytesResponse LoadFont keepBytes
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
    { count = 0, font = Nothing, fontData = Nothing }


emptyBytes =
    Encode.encode (Encode.sequence [])


dropBytesDecoder toDrop buffer =
    Decode.succeed (\_ v -> v)
        |> andMap (Decode.bytes toDrop)
        |> andMap (Decode.bytes (Bytes.width buffer - toDrop))


dropBytes toDrop buffer =
    case Decode.decode (dropBytesDecoder toDrop buffer) buffer of
        Nothing ->
            emptyBytes

        Just v ->
            v


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadFont (Err e) ->
            let
                _ =
                    Debug.log "error in decoding bytes" e
            in
            ( model, Cmd.none )

        LoadFont (Ok rawBytes) ->
            let
                font =
                    Woff2.decode (dropBytes currentFont.offset rawBytes)

                cmap =
                    Maybe.andThen Font.characterMapping font

                cff =
                    Maybe.map2 (\set buffer -> CompactFontFormat.pickFont buffer set 0) (Maybe.andThen Font.compactFontFormat font) cffBytes
                        |> Maybe.andThen identity

                header =
                    Maybe.andThen Font.header font

                -- subs = Maybe.andThen Font.glyphSubstitution font
                advanceWidths =
                    Maybe.andThen Font.advanceWidths font

                fontIndex =
                    -- which font in the file
                    0

                cffBytes =
                    Maybe.andThen (Dict.get "CFF " << .tables) font
            in
            ( { model
                | font = font
                , fontData = Maybe.map4 FontData cff cmap advanceWidths (Maybe.map .boundingBox header)
              }
            , Cmd.none
            )


googleFonts =
    Html.div []
        [ Html.node "style"
            []
            [ Html.text """.kafli1 { font-family: 'Source Sans Pro', sans-serif; font-style: italic; font-weight: 400; font-size: 14px;}"""
            ]
        , Html.node "link" [ Html.Attributes.href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i", Html.Attributes.rel "stylesheet" ] []

        --        , Html.node "link" [ Html.Attributes.href "http://0.0.0.0:8001/SourceSansPro-It.otf", Html.Attributes.rel "stylesheet" ] []
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "yay"
    , body =
        [ googleFonts
        , div [ Html.Attributes.style "background-color" "steelblue" ] <|
            case model.font of
                Nothing ->
                    [ text "no font loaded" ]

                Just font ->
                    let
                        row a b =
                            tr [] [ td [] [ text a ], td [] [ b ] ]

                        rec r =
                            row (Debug.toString r.tag) (text (Debug.toString r.offset))
                    in
                    [ text "font loaded" ]

        {-
           , Svg.svg [ width "5000", height "1000" ]
               [ case model.fontData of
                   Just fontData ->
                       Svg.g []
                           [ allGlyphs fontData

                           -- , Svg.g [ transform "translate(0, 100)" ] [ renderString { offsetY = 0 } "Og sáu þeir það ekki; smörgås; bjørn; weiß" fontData ]
                           , let
                               unitsPerEm =
                                   model.font
                                       |> Maybe.andThen Font.header
                                       |> Maybe.map .unitsPerEm
                                       |> Maybe.withDefault 0
                                       |> Debug.log "units per em"

                               convert { pixels, width } =
                                   floor ((width / pixels) * toFloat unitsPerEm)

                               w =
                                   convert { pixels = 16, width = 500 }

                               lines =
                                   List.map (Paragraph.lines { maximumWidth = w, optimalWidth = round (1.0 * toFloat w), stringWidth = stringWidth fontData.cmap fontData.advanceWidths }) paragraphs
                                       |> List.take 1
                                       |> Debug.log "lines"

                               string =
                                   " Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en"

                               expected =
                                   (toFloat (stringWidth fontData.cmap fontData.advanceWidths string) / 1000) * 16

                               _ =
                                   Debug.log "expected width" expected
                             in
                             -- Svg.g [] (List.map (viewString unitsPerEm fontData fontData.glyphBoundingBox) [ [ string ] ])
                             Svg.g [] (List.map (viewString unitsPerEm fontData fontData.glyphBoundingBox) lines)

                           -- lines)
                           {-
                              , let
                                  w =
                                      round (80 * 14 * (1 / 1000))

                                  lines =
                                      List.map (Paragraph.lines { maximumWidth = w, optimalWidth = round (0.9 * toFloat w), stringWidth = stringWidth fontData.cmap fontData.advanceWidths }) paragraphs
                                          |> List.take 1
                                in
                                Svg.g [ transform "translate(0, 200) " ]
                                  [ renderStringAsString (fontData.glyphBoundingBox.yMax - fontData.glyphBoundingBox.yMin) (List.map (String.join " ") lines)
                                  ]
                           -}
                           ]

                   Nothing ->
                       text "no font data"
               ]
        -}
        ]
    }


glyphForCharacter : CharacterMapping -> Glyphs -> Char -> Charstring.Charstring
glyphForCharacter cmap glyphs character =
    Tables.CharacterMapping.getGlyphIndex cmap 0 character
        |> (\gid -> Glyphs.charstring gid glyphs)


characterName : FontData -> Char -> String
characterName fontData char =
    let
        encoding character =
            Tables.CharacterMapping.getGlyphIndex fontData.cmap 0 character
    in
    CompactFontFormat.characterToName fontData.cff encoding char


allGlyphs : FontData -> Svg msg
allGlyphs fontData =
    let
        glyphs : Array Int
        glyphs =
            case fontData.cmap.encodings |> Array.get 0 of
                Just (Format0 encoding) ->
                    encoding.glyphIdArray

                Just (Format4 encoding) ->
                    encoding.glyphIdArray

                Just (Format6 encoding) ->
                    encoding.glyphIdArray

                _ ->
                    Array.empty

        makePath instructions offset =
            let
                pathString =
                    Render.glyph instructions
            in
            Svg.path [ d pathString, fill "black", stroke "black", strokeWidth "0", transform ("scale(0.02, -0.02)  translate(" ++ String.fromInt offset ++ ", -750)") ] []

        go : Maybe ( Charstring.Charstring, Int ) -> ( Int, List (Svg msg) ) -> ( Int, List (Svg msg) )
        go element ( offset, accum ) =
            case element of
                Just ( instructions, width ) ->
                    ( offset + width
                    , makePath instructions offset :: accum
                    )

                Nothing ->
                    ( offset, accum )
    in
    glyphs
        |> Array.map (\index -> Maybe.map2 Tuple.pair (Just <| Glyphs.charstring index fontData.cff.glyphs) (Array.get index fontData.advanceWidths))
        |> Array.foldl go ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> Svg.g [ transform "translate(0, 10)" ]


viewString : Int -> FontData -> BoundingBox -> List String -> Svg msg
viewString unitsPerEm fontData boundingBox lines =
    let
        height =
            boundingBox.yMax - boundingBox.yMin

        _ =
            Debug.log "line height" boundingBox
    in
    List.indexedMap (\i words -> renderString { unitsPerEm = unitsPerEm, height = height, offsetY = i * height } words fontData) lines
        |> Svg.g []


stringWidth : CharacterMapping -> Array Int -> String -> Int
stringWidth cmap advanceWidths string =
    let
        glyphIndex character =
            Tables.CharacterMapping.getGlyphIndex cmap 0 character

        widthForCharacter character =
            Array.get (glyphIndex character) advanceWidths
                |> Maybe.withDefault 0
    in
    String.foldl (\c accum -> widthForCharacter c + accum) 0 string


renderStringAsString : Int -> List String -> Svg msg
renderStringAsString height lines =
    let
        makePath line offsetX offsetY =
            Svg.text_
                [ fill "black"
                , stroke "none"
                , Svg.Attributes.class "kafli1"

                --, Svg.Attributes.fontSize "1.24em"
                , Svg.Attributes.fontSize "1em"

                --, transform ("translate(" ++ String.fromInt (offsetX // 1000) ++ ", " ++ String.fromInt ((-750 - offsetY) // 1000) ++ " )")
                , transform ("translate(" ++ String.fromInt 0 ++ ", " ++ String.fromInt (230 + offsetY // 1000 * 16) ++ ")")
                ]
                [ Svg.text line ]
    in
    List.indexedMap (\i line -> makePath line 0 (i * height + height // 2)) lines
        |> Svg.g []


scale { x, y } =
    "scale(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


renderString : { unitsPerEm : Int, height : Int, offsetY : Int } -> String -> FontData -> Svg msg
renderString { offsetY, height, unitsPerEm } str fontData =
    let
        characters =
            String.toList str

        makePath instructions offsetX =
            let
                pathString =
                    Render.glyph instructions

                -- _ = Debug.log "offsets" ( offsetX // 1000, offsetY // 1000, pathString )
                scaleGlyph =
                    scale { x = 1 * (1 / toFloat unitsPerEm), y = -1 * (1 / toFloat unitsPerEm) }

                moveGlyph =
                    "translate(" ++ String.fromFloat (toFloat offsetX / toFloat unitsPerEm) ++ ", " ++ String.fromFloat (toFloat offsetY / toFloat unitsPerEm) ++ " )"
            in
            Svg.path
                [ d pathString
                , fill "black"
                , stroke "black"
                , strokeWidth "0"

                -- , transform ("scale(0.01, -0.01)  translate(" ++ String.fromInt offsetX ++ ", " ++ String.fromInt (-750 - offsetY) ++ " )")
                , transform (moveGlyph ++ scaleGlyph)
                ]
                []

        go character ( offset, accum ) =
            let
                instructions =
                    glyphForCharacter fontData.cmap fontData.cff.glyphs character

                index =
                    Tables.CharacterMapping.getGlyphIndex fontData.cmap 0 character

                gid =
                    Char.toCode character

                width =
                    Array.get index fontData.advanceWidths
                        |> Maybe.withDefault 0
            in
            ( offset + width
            , makePath instructions offset :: accum
            )
    in
    List.foldl go ( 0, [] ) characters
        |> Tuple.second
        |> List.reverse
        |> Svg.g [ transform <| "translate(0, " ++ String.fromInt (2 * (height // unitsPerEm)) ++ ") " ]
        |> List.singleton
        |> Svg.g [ transform <| "translate(0, 50) scale(16, 16)" ]


renderGlyph index glyphs offset =
    let
        subroutine =
            Glyphs.charstring index glyphs

        subpaths =
            Render.glyph subroutine
    in
    Svg.path [ d subpaths, fill "none", stroke "black", strokeWidth "2", transform ("scale(1, -1)  translate(" ++ String.fromInt offset ++ ", -750)") ] []


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, loadFont )
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
                case version of
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


paragraphs =
    String.split "\n\n" kafli1
        |> List.map (String.replace "\n" "")


kafli1 =
    """Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum.

Svo segja vitrir menn, að úr Noregi frá Staði sé sjö dægra sigling í vestur til Horns á Íslandi austanverðu, en frá Snæfellsnesi, þar er skemmst er, er fjögurra dægra haf í vestur til Grænlands. En svo er sagt, ef siglt er úr Björgyn rétt í vestur til Hvarfsins á Grænlandi, að þá mun siglt vera tylft fyrir sunnan Ísland. Frá Reykjanesi á sunnanverðu Íslandi er fimm dægra haf til Jölduhlaups á Írlandi (í suður; en frá Langanesi á norðanverðu Íslandi er) fjögurra dægra haf norður til Svalbarða í hafsbotn.

Svo er sagt, að menn skyldu fara úr Noregi til Færeyja; nefna sumir til Naddodd víking; en þá rak vestur í haf og fundu þar land mikið. Þeir gengu upp í Austfjörðum á fjall eitt hátt og sáust um víða, ef þeir sæju reyki eða nokkur líkindi til þess, að landið væri byggt, og sáu þeir það ekki.

Þeir fóru aftur um haustið til Færeyja; og er þeir sigldu af landinu, féll snær mikill á fjöll, og fyrir það kölluðu þeir landið Snæland. Þeir lofuðu mjög landið.

Þar heitir nú Reyðarfjall í Austfjörðum, er þeir höfðu að komið. Svo sagði Sæmundur prestur hinn fróði.

Maður hét Garðar Svavarsson, sænskur að ætt; hann fór að leita Snælands að tilvísan móður sinnar framsýnnar. Hann kom að landi fyrir austan Horn hið eystra; þar var þá höfn. Garðar sigldi umhverfis landið og vissi, að það var eyland. Hann var um veturinn norður í Húsavík á Skjálfanda og gerði þar hús.

Um vorið, er hann var búinn til hafs, sleit frá honum mann á báti, er hét Náttfari, og þræl og ambátt. Hann byggði þar síðan, er heitir Náttfaravík.

Garðar fór þá til Noregs og lofaði mjög landið. Hann var faðir Una, föður Hróars Tungugoða. Eftir það var landið kallað Garðarshólmur, og var þá skógur milli fjalls og fjöru.
"""
