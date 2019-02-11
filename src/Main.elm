module Main exposing (main)

import Browser
import Buffalo exposing (buffalo)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Tree exposing (RenderedNode, Tree(..), sampleTree)


type Msg
    = ShowNodeTooltip RenderedNode
    | HideNodeTooltip
    | UpdateNumBuffalo String


type alias Model =
    { currentTooltip : Maybe RenderedNode
    , numBuffalo : Int
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
                [ renderNode node, text "buffalo" ]


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

        UpdateNumBuffalo input ->
            ( { model | numBuffalo = String.toInt input |> Maybe.withDefault 0 }, Cmd.none )


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
        [ div [] (renderTrees (buffalo model.numBuffalo))
        , input
            [ type_ "range"
            , Attr.min "0"
            , Attr.max "20"
            , value <| String.fromInt model.numBuffalo
            , onInput UpdateNumBuffalo
            ]
            []
        , div [ class "controls" ]
            [ div [] [ text ("num buffalo: " ++ String.fromInt model.numBuffalo) ]
            , div [] [ div [] [ text "currentNode:" ], div [] [ text currentTooltipText ] ]
            ]
        ]


init : Flags -> ( Model, Cmd Msg )
init opts =
    ( { currentTooltip = Nothing
      , numBuffalo = 3
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
