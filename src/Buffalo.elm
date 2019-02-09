module Buffalo exposing (buffalo)

import Tree exposing (RenderedNode, Tree(..))



-- live demo : swap out character set
-- live demo : imperative tense

type
    SemanticValue
    -- Could do some real fun stuff with type aliases and parts of speech
    = N ((String -> String) -> String) -- it's either with an adjective or by itself
    | NP String
    | AdjP (String -> String)
    | IntrVP (String -> String)
    | S String


type alias BuffaloExpression =
    { semantics : SemanticValue
    , tree : Tree
    }



-- Function application builds up tree and parses expressions


buffaloNP : BuffaloExpression
buffaloNP =
    { semantics = NP "[the group of mammals within the subfamily Bovinae]"
    , tree = TerminalNode (RenderedNode "NP" "[the group of mammals within the subfamily Bovinae]")
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

        IntrVP predicate ->
            "VP"

        S sentence ->
            "S"


buffaloN : BuffaloExpression
buffaloN =
    { semantics = N (\p -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x")
    , tree = TerminalNode (RenderedNode "N" "\\x[x is a member of the group of mammals within the subfamily Bovinae]")
    }


buffaloCity : BuffaloExpression
buffaloCity =
    { semantics = AdjP (\x -> "[" ++ x ++ " is from Buffalo]")
    , tree = TerminalNode (RenderedNode "AdjP" "\\x[x is from Buffalo]")
    }


buffaloIntrVerb : BuffaloExpression
buffaloIntrVerb =
    { semantics = IntrVP (\x -> "[" ++ x ++ " bullies (someone)]")
    , tree = TerminalNode (RenderedNode "VP" "\\x[x bullies (someone)]")
    }



-- exprToRenderedNode : BuffaloExpression -> RenderedNode
-- exprToRenderedNode ({ surface, semantics } as expr) =
--     case semantics of
--         NP individual ->
--             RenderedNode "NP" surface
--         N predicate ->
--             RenderedNode "N" surface
--         AdjP descriptor ->
--             RenderedNode "AdjP" surface


toRenderTrees : List BuffaloExpression -> List Tree
toRenderTrees buffaloExprs =
    List.map .tree buffaloExprs


expressionApplication : BuffaloExpression -> List BuffaloExpression
expressionApplication ({ tree, semantics } as expr) =
    case semantics of
        -- Given our very limited set of tokens, we know that N will only occur with AdjP
        -- We further also know that there is currently only one AdjP
        N predicate ->
            case buffaloCity.semantics of
                AdjP descriptor ->
                    [ { semantics = NP (predicate descriptor)
                          , tree = Node ( RenderedNode "NP" (predicate descriptor), [ buffaloCity.tree, tree ] )
                          }
                        ]

                _ ->
                    []

        IntrVP predicate ->
            let
                sentence =
                    case buffaloNP.semantics of
                        NP individual ->
                            [ { semantics = S (predicate individual)
                              , tree = Node ( RenderedNode "S" (predicate individual), [ buffaloNP.tree, tree ] )
                              }
                            ]

                        _ ->
                            []
            in
                sentence

        _ ->
            []


buffalo : Int -> List Tree
buffalo num =
    case num of
        0 ->
            []

        1 ->
            buffaloParser num |> toRenderTrees

        2 ->
            buffaloParser 1
                |> List.map expressionApplication
                |> List.concat
                |> toRenderTrees

        _ ->
            []


buffaloParser : Int -> List BuffaloExpression
buffaloParser num =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN, buffaloCity, buffaloNP, buffaloIntrVerb ]

        _ ->
            []
