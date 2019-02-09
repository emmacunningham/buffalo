module Buffalo exposing (buffalo)

import Tree exposing (RenderedNode, Tree(..))



-- type SyntacticCategory
--     = N
--     | NP
--     | TransitiveVerb
--     | IntransitiveVerb
--     | AdjP


type SemanticValue
    = N ((String -> String) -> String)
    | NP String
    | AdjP (String -> String)


type alias BuffaloExpression =
    { surface : String
    , semantics : SemanticValue
    , tree : Tree
    }



-- Function application builds up tree and parses expressions


buffaloNP : BuffaloExpression
buffaloNP =
    { surface = "the group of mammals within the subfamily Bovinae"
    , semantics = NP "the group of mammals within the subfamily Bovinae"
    , tree = TerminalNode (RenderedNode "NP" "the group of mammals within the subfamily Bovinae")
    }



-- noun : String -> (String -> String) -> String
-- noun predicate adj =
--     \px -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x"


convertSyntacticCategoryToString : SemanticValue -> String
convertSyntacticCategoryToString expr =
    case expr of
        NP individual ->
            "NP"

        N predicate ->
            "N"

        AdjP descriptor ->
            "AdjP"


buffaloN : BuffaloExpression
buffaloN =
    { surface = "\\x[x is a member of the group of mammals within the subfamily Bovinae]"
    , semantics = N (\p -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x")
    , tree = TerminalNode (RenderedNode "N" "\\x[x is a member of the group of mammals within the subfamily Bovinae]")
    }


buffaloCity : BuffaloExpression
buffaloCity =
    { surface = "\\P[the x such that x is from Buffalo and P(x)]"
    , semantics = AdjP (\x -> "x is from Buffalo")
    , tree = TerminalNode (RenderedNode "AdjP" "\\P[the x such that x is from Buffalo and P(x)]")
    }


exprToRenderedNode : BuffaloExpression -> RenderedNode
exprToRenderedNode ({ surface, semantics } as expr) =
    case semantics of
        NP individual ->
            RenderedNode "NP" surface

        N predicate ->
            RenderedNode "N" surface

        AdjP descriptor ->
            RenderedNode "AdjP" surface


toRenderTrees : List BuffaloExpression -> List Tree
toRenderTrees buffaloExprs =
    List.map .tree buffaloExprs


expressionApplication : BuffaloExpression -> Maybe BuffaloExpression
expressionApplication ({ tree, semantics } as expr) =
    case semantics of
        N predicate ->
            Just
                { surface = "not sure this even matters anymore"
                , semantics = NP "the x s.t. is a member of the group of mammals within the subfamily Bovinae and x is from Buffalo, NY"
                , tree = Node ( RenderedNode "NP" "the x s.t. is a member of the group of mammals within the subfamily Bovinae and x is from Buffalo, NY", [ buffaloCity.tree, tree ] )
                }

        _ ->
            Nothing


buffalo : Int -> List Tree
buffalo num =
    case num of
        0 ->
            []

        1 ->
            buffaloParser num |> toRenderTrees

        2 ->
            buffaloParser 1
                |> List.filterMap expressionApplication
                |> toRenderTrees

        _ ->
            []


buffaloParser : Int -> List BuffaloExpression
buffaloParser num =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN, buffaloCity, buffaloNP ]

        _ ->
            []
