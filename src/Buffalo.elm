module Buffalo exposing (buffalo)

import Tree exposing (RenderedNode, Tree(..))



-- live demo : swap out character set
-- live demo : imperative tense


type alias Subject =
    String


type alias Object =
    String


type alias Sentence =
    String


type alias TruthStatement =
    String


type alias Adjective =
    String -> String


type alias Individual =
    String


type SemanticValue
    = N ((Individual -> TruthStatement) -> TruthStatement) -- it's either with an adjective or by itself
    | NP Individual
    | AdjP (Individual -> TruthStatement)
    | S Sentence
    | TrVerb (Object -> Subject -> Sentence)
    | VP (Subject -> Sentence)


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
    { semantics = VP (\x -> "[" ++ x ++ " bullies (someone)]")
    , tree = TerminalNode (RenderedNode "VP" "\\x[x bullies (someone)]")
    }


buffaloTrVerb : BuffaloExpression
buffaloTrVerb =
    { semantics = TrVerb (\o s -> "[" ++ s ++ " bullies" ++ o ++ "]")
    , tree = TerminalNode (RenderedNode "Verb" "\\o s[s bullies o]")
    }


toRenderTrees : List BuffaloExpression -> List Tree
toRenderTrees buffaloExprs =
    List.map .tree buffaloExprs


isNP : BuffaloExpression -> Bool
isNP ({ semantics } as expr) =
    case semantics of
        NP _ ->
            True

        _ ->
            False


complexBuffaloNP : List BuffaloExpression
complexBuffaloNP =
    buffaloParser 2
        |> List.filter isNP



-- TODO: make expression application take a list of expressions
-- and use the list of expressions as a list of next possible expressions
-- num next tells you how many next expressions to take


applyVP : Tree -> (Subject -> Sentence) -> BuffaloExpression -> Maybe BuffaloExpression
applyVP curTree predicate next =
    case next.semantics of
        NP individual ->
            Just
                { semantics = S (predicate individual)
                , tree = Node ( RenderedNode "S" (predicate individual), [ next.tree, curTree ] )
                }

        _ ->
            Nothing


expressionApplication : Bool -> BuffaloExpression -> List BuffaloExpression
expressionApplication skipNext ({ tree, semantics } as expr) =
    case ( semantics, skipNext ) of
        -- Given our very limited set of tokens, we know that N will only occur with AdjP
        -- We further also know that there is currently only one AdjP
        ( N predicate, False ) ->
            case buffaloCity.semantics of
                AdjP descriptor ->
                    [ { semantics = NP (predicate descriptor)
                      , tree = Node ( RenderedNode "NP" (predicate descriptor), [ buffaloCity.tree, tree ] )
                      }
                    ]

                _ ->
                    []

        ( VP predicate, False ) ->
            let
                sentence =
                    List.filterMap (applyVP tree predicate) (buffaloParser 1)

                -- case buffaloNP.semantics of
                --     NP individual ->
                --         [ { semantics = S (predicate individual)
                --           , tree = Node ( RenderedNode "S" (predicate individual), [ buffaloNP.tree, tree ] )
                --           }
                --         ]
                --     _ ->
                --         []
            in
            sentence

        ( VP predicate, True ) ->
            List.filterMap (applyVP tree predicate) (buffaloParser 2)

        ( NP individual, False ) ->
            case buffaloTrVerb.semantics of
                TrVerb predicate ->
                    [ { semantics = VP (predicate individual)
                      , tree = Node ( RenderedNode "VP" ("\\x" ++ predicate individual "x"), [ buffaloTrVerb.tree, tree ] )
                      }
                    ]

                _ ->
                    []

        ( _, _ ) ->
            []


buffalo : Int -> List Tree
buffalo num =
    case num of
        0 ->
            []

        1 ->
            buffaloParser 1 |> toRenderTrees

        _ ->
            buffaloParser num |> toRenderTrees



-- What we want to do is be able to pass in the next expression
-- so that for [buffalo 3], we can parse it as [buffalo 1] [buffalo 2]
-- as well as [[buffalo 1] [buffalo 1]] [buffalo 1] (this is what we currently support)
-- solve for our previous case first and then we'll  work to support the first case


buffaloParser : Int -> List BuffaloExpression
buffaloParser num =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN, buffaloCity, buffaloNP, buffaloIntrVerb, buffaloTrVerb ]

        2 ->
            buffaloParser (num - 1)
                |> List.map (expressionApplication False)
                |> List.concat

        _ ->
            let
                nextDirect =
                    buffaloParser (num - 1)
                        |> List.map (expressionApplication False)
                        |> List.concat

                skipNext =
                    buffaloParser (num - 2)
                        |> List.map (expressionApplication True)
                        |> List.concat
            in
            List.concat [ nextDirect, skipNext ]
