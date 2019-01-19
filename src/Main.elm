module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)


type Msg
    = Increment
    | Decrement
    | UpdateInput String


type alias Model =
    {}


type alias Flags =
    {}


type alias RenderedNode =
    { label : String
    , details : String
    }


type Tree
    = Node ( RenderedNode, List Tree )


sampleTree : Tree
sampleTree =
    let
        a =
            RenderedNode "A" "A node"

        b =
            RenderedNode "B" "B node"

        c =
            RenderedNode "C" "C node"

        d =
            RenderedNode "D" "D node"

        e =
            RenderedNode "E" "E node"
    in
    Node
        ( a
        , [ Node ( b, [] )
          , Node
                ( c
                , [ Node ( d, [] )
                  , Node ( e, [] )
                  ]
                )
          ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "foo"
        ]


init : Flags -> ( Model, Cmd Msg )
init opts =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
