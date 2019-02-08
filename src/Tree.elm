module Tree exposing (RenderedNode, Tree(..), sampleTree)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)

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
