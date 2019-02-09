module Main exposing (main)

import Browser
import Buffalo exposing (buffalo)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Tree exposing (RenderedNode, Tree(..), sampleTree)


type Msg
    = ShowNodeTooltip RenderedNode
    | HideNodeTooltip


type alias Model =
    { currentTooltip : Maybe RenderedNode
    }


type alias Flags =
    {}


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
            div [ class "tree" ]
                [ renderNode parent
                , renderLines
                , div [ class "nodes" ] (List.map renderTree children)
                ]

        TerminalNode node ->
            div [ class "tree" ]
                [ renderNode node, renderLines ]


renderTreeContainer : Tree -> Html Msg
renderTreeContainer tree =
    div [ class "tree-container" ]
        [ renderTree tree
        ]


renderTrees : List Tree -> List (Html Msg)
renderTrees trees =
    List.map renderTreeContainer trees


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
        [ div [] (renderTrees (buffalo 2))
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
