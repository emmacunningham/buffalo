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
    | ObjNP ((String -> (String -> String)) -> String -> String)
    | TrVerb (String -> String -> String)
    | VP (String -> String)


type alias BuffaloExpression =
    { semantics : SemanticValue
    , tree : Tree
    }



-- Function application builds up tree and parses expressions


buffaloMammalGroup : String
buffaloMammalGroup =
    "[the group of mammals within the subfamily Bovinae]"


buffaloNP : BuffaloExpression
buffaloNP =
    { semantics = NP buffaloMammalGroup
    , tree = TerminalNode (RenderedNode "NP" buffaloMammalGroup)
    }



-- noun : String -> (String -> String) -> String
-- noun predicate adj =
--     \px -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x"
-- convertSyntacticCategoryToString : SemanticValue -> String
-- convertSyntacticCategoryToString expr =
--     case expr of
--         NP _ ->
--             "NP"
--         N _ ->
--             "N"
--         AdjP _ ->
--             "AdjP"
--         IntrVP _ ->
--             "VP"
--         S _ ->
--             "S"
--         ObjNP _ ->
--             "ObjNP"
--         TrVerb _ ->
--             "Verb"


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


buffaloTrVerb : BuffaloExpression
buffaloTrVerb =
    { semantics = TrVerb (\o s -> "[" ++ s ++ " bullies" ++ o ++ "]")
    , tree = TerminalNode (RenderedNode "Verb" "\\o s[s bullies o]")
    }


buffaloObjNP : BuffaloExpression
buffaloObjNP =
    { semantics = ObjNP (\p x -> p buffaloMammalGroup x)
    , tree = buffaloNP.tree
    }


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

        ObjNP individual ->
            case buffaloTrVerb.semantics of
                TrVerb predicate ->
                    [ { semantics = VP (individual predicate)
                      , tree = Node ( RenderedNode "VP" ("\\x" ++ individual predicate "x"), [ buffaloTrVerb.tree, tree ] )
                      }
                    ]

                _ ->
                    []

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
            [ buffaloN, buffaloCity, buffaloNP, buffaloIntrVerb, buffaloObjNP, buffaloTrVerb ]

        _ ->
            []
