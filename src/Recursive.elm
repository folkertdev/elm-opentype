module Recursive exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        , text (Debug.toString ( facHelper1, facHelper2 ))
        ]


fac2 n =
    facHelper1 n 1


facHelper1 n accum =
    if n == 1 then
        accum

    else
        facHelper1 (n - 1) (n * accum)


facHelper2 =
    \n accum ->
        if n == 1 then
            accum

        else
            facHelper2 (n - 1) (n * accum)


x =
    let
        fac n =
            facHelper n 1

        facHelper n accum =
            if n == 1 then
                accum

            else
                facHelper (n - 1) (n * accum)
    in
    fac 4


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
