module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)


type Msg
    = ShowNodeTooltip RenderedNode
    | HideNodeTooltip


type alias Model =
    { currentTooltip : Maybe RenderedNode
    }


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


renderNode : RenderedNode -> Html Msg
renderNode node =
    div [ onMouseOver (ShowNodeTooltip node), onMouseOut HideNodeTooltip ] [ text node.label ]


renderLines : Html Msg
renderLines =
    div [] [ text "--------" ]


renderTree : Tree -> Html Msg
renderTree tree =
    case tree of
        Node ( parent, children ) ->
            div []
                [ renderNode parent
                , renderLines
                , div [ class "nodes" ] (List.map renderTree children)
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowNodeTooltip node ->
            ( { model | currentTooltip = Just node }, Cmd.none )

        HideNodeTooltip ->
            ( { model | currentTooltip = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        currentTooltipText =
            case model.currentTooltip of
                Nothing ->
                    ""

                Just node ->
                    node.details
    in
    div []
        [ renderTree sampleTree
        , text ("currentNode: " ++ currentTooltipText)
        ]


init : Flags -> ( Model, Cmd Msg )
init opts =
    ( { currentTooltip = Nothing
      }
    , Cmd.none
    )


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
