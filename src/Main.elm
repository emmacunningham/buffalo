module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)


type Msg
    = Increment
    | Decrement
    | UpdateInput String


type alias Model =
    { curCalcValue : Int
    , curInputString : String
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            let
                incValue =
                    Maybe.withDefault 5 (String.toInt model.curInputString)
            in
            { model | curCalcValue = model.curCalcValue + incValue }

        Decrement ->
            { model | curCalcValue = model.curCalcValue - 5 }

        UpdateInput value ->
            { model | curInputString = value }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.curCalcValue) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ input [ onInput UpdateInput ] [] ]
        , text model.curInputString
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { curCalcValue = 0, curInputString = "" }
        , update = update
        , view = view
        }
