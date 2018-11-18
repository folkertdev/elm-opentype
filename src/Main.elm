module Main exposing (main)

import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { count : Int }


openSans : Cmd Msg
openSans =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept-Ranges" "bytes" ]
        , url = "../MontserratAlternates-Black.otf"
        , body = Http.emptyBody
        , expect = Http.expectBytes OpenSans openFontDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement
    | OpenSans (Result Http.Error Font)


type alias Font =
    { p1 : Int
    , p2 : Int
    , p3 : Int
    , p4 : Int
    , p5 : Int
    , p6 : Int
    , p7 : Int
    }


andMap first later =
    Decode.map2 (<|) later first


openFontDecoder =
    Decode.succeed Font
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap Decode.unsignedInt8
        |> andMap Decode.unsignedInt8


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
                x : Maybe Int
                x =
                    Encode.encode (Encode.unsignedInt32 BE 42)
                        |> Decode.decode (Decode.unsignedInt32 BE)

                _ =
                    Debug.log "roundtrip" x

                _ =
                    Debug.log "error in decoding bytes" e
            in
            ( model, Cmd.none )

        OpenSans (Ok file) ->
            let
                _ =
                    Debug.log "decoded " file
            in
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "yay"
    , body =
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, openSans )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
