module Main exposing (main)

import Browser
import Buffalo exposing (buffalo)
import Expression exposing (ExpressionFilter(..), Representation(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Tree exposing (RenderedNode, Tree(..), sampleTree)


type Msg
    = ShowNodeTooltip RenderedNode
    | HideNodeTooltip
    | UpdateNumBuffalo String
    | UpdateExpressionFilter ExpressionFilter
    | UpdateRepresentation Representation


type alias Model =
    { currentTooltip : Maybe RenderedNode
    , numBuffalo : Int
    , parseType : ExpressionFilter
    , representation : Representation
    }


type alias Flags =
    {}


renderNode : RenderedNode -> Html Msg
renderNode node =
    div [ onMouseOver (ShowNodeTooltip node), onMouseOut HideNodeTooltip, class "node-label" ] [ text node.label ]


renderLines : Html Msg
renderLines =
    div [] [ text "--------" ]


renderTerminalNode : RenderedNode -> String
renderTerminalNode node =
    case node.label of
        "wh" ->
            "(that)"

        _ ->
            "buffalo"


renderTree : Tree -> Html Msg
renderTree tree =
    case tree of
        Node ( parent, children ) ->
            div [ class "tree" ]
                [ renderNode parent
                , div [ class "nodes" ] (List.map renderTree children)
                ]

        TerminalNode node ->
            div [ class "tree" ]
                [ renderNode node, text (renderTerminalNode node) ]


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

        UpdateExpressionFilter filter ->
            ( { model | parseType = filter }, Cmd.none )

        UpdateRepresentation rep ->
            ( { model | representation = rep }, Cmd.none )


renderExpressionFilters : Model -> Html Msg
renderExpressionFilters model =
    let
        isAllActive =
            case model.parseType of
                All ->
                    True

                Sentences ->
                    False
    in
    div [ class "expression-filters" ]
        [ renderExpressionFilter isAllActive All "all phrases"
        , renderExpressionFilter (not isAllActive) Sentences "only sentences"
        ]


renderExpressionFilter : Bool -> ExpressionFilter -> String -> Html Msg
renderExpressionFilter isActive filter label =
    let
        classes =
            case isActive of
                True ->
                    "active filter"

                False ->
                    "filter"
    in
    div [ onClick (UpdateExpressionFilter filter), class classes ] [ text label ]


renderRepresentationSelectors : Model -> Html Msg
renderRepresentationSelectors model =
    let
        isVerboseActive =
            case model.representation of
                Verbose ->
                    True

                _ ->
                    False
    in
    div [ class "representation-types" ]
        [ renderRepresentationSelector isVerboseActive Verbose "verbose"
        , renderRepresentationSelector (not isVerboseActive) Emoji "emoji"
        ]


renderRepresentationSelector : Bool -> Representation -> String -> Html Msg
renderRepresentationSelector isActive rep label =
    let
        classes =
            case isActive of
                True ->
                    "active rep"

                False ->
                    "rep"
    in
    div [ onClick (UpdateRepresentation rep), class classes ] [ text label ]


renderControls : Model -> Int -> Html Msg
renderControls model numExprs =
    let
        currentTooltipText =
            case model.currentTooltip of
                Nothing ->
                    ""

                Just node ->
                    node.details

        parseType =
            case model.parseType of
                All ->
                    "phrases"

                Sentences ->
                    "sentences"
    in
    div [ class "controls" ]
        [ input
            [ type_ "range"
            , Attr.min "0"
            , Attr.max "20"
            , value <| String.fromInt model.numBuffalo
            , onInput UpdateNumBuffalo
            ]
            []
        , div [ class "options" ]
            [ renderExpressionFilters model
            , renderRepresentationSelectors model
            ]
        , div [] [ text ("buffalo(" ++ String.fromInt model.numBuffalo ++ ") yields " ++ String.fromInt numExprs ++ " valid " ++ parseType) ]
        , div [] [ div [ class "details-container" ] [ text "current node: " ], div [ class "node-details" ] [ text currentTooltipText ] ]
        ]


view : Model -> Html Msg
view model =
    let
        buffaloExprs =
            buffalo model.numBuffalo model.parseType model.representation
    in
    div []
        [ renderControls model (List.length buffaloExprs)
        , div [ class "trees-container" ] (renderTrees buffaloExprs)
        ]


init : Flags -> ( Model, Cmd Msg )
init opts =
    ( { currentTooltip = Nothing
      , numBuffalo = 3
      , parseType = Sentences
      , representation = Verbose
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
