module Buffalo exposing (buffalo)

import ExpressionFilter exposing (ExpressionFilter(..))
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
    | S TruthStatement
    | TrVerb (Object -> Individual -> TruthStatement)
    | VP (Individual -> TruthStatement)


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


attachRC : Individual -> (Individual -> TruthStatement) -> Individual
attachRC individual predicate =
    predicate (individual ++ " who ")


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


rcNode : Tree
rcNode =
    TerminalNode (RenderedNode "wh" "(that)")


transformVP : Tree -> (Subject -> Sentence) -> BuffaloExpression -> Maybe BuffaloExpression
transformVP curTree predicate next =
    case next.semantics of
        NP individual ->
            Just
                { semantics = NP (attachRC individual predicate)
                , tree =
                    Node
                        ( RenderedNode "NP-RC" (attachRC individual predicate)
                        , [ next.tree, Node ( RenderedNode "RC" (predicate "that"), [ rcNode, curTree ] ) ]
                        )
                }

        _ ->
            Nothing


applyNP : Tree -> Individual -> BuffaloExpression -> Maybe BuffaloExpression
applyNP curTree individual next =
    case next.semantics of
        TrVerb predicate ->
            Just
                { semantics = VP (predicate individual)
                , tree = Node ( RenderedNode "VP" ("\\x" ++ predicate individual "x"), [ next.tree, curTree ] )
                }

        _ ->
            Nothing


applyN : Tree -> ((Individual -> TruthStatement) -> TruthStatement) -> BuffaloExpression -> Maybe BuffaloExpression
applyN curTree predicate next =
    case next.semantics of
        AdjP descriptor ->
            Just
                { semantics = NP (predicate descriptor)
                , tree = Node ( RenderedNode "NP" (predicate descriptor), [ next.tree, curTree ] )
                }

        _ ->
            Nothing


expressionApplication : Int -> BuffaloExpression -> List BuffaloExpression
expressionApplication takeNext ({ tree, semantics } as expr) =
    case semantics of
        N predicate ->
            List.filterMap (applyN tree predicate) (buffaloParser takeNext)

        VP predicate ->
            let
                sentence =
                    List.filterMap (applyVP tree predicate) (buffaloParser takeNext)

                relativeClause =
                    List.filterMap (transformVP tree predicate) (buffaloParser takeNext)
            in
            List.concat [ sentence, relativeClause ]

        NP individual ->
            List.filterMap (applyNP tree individual) (buffaloParser takeNext)

        _ ->
            []


toRenderTrees : ExpressionFilter -> List BuffaloExpression -> List Tree
toRenderTrees filter buffaloExprs =
    let
        filterFn =
            case filter of
                All ->
                    allFilter

                Sentences ->
                    sentenceFilter
    in
    buffaloExprs
        |> List.filterMap filterFn


allFilter : BuffaloExpression -> Maybe Tree
allFilter expr =
    Just expr.tree


sentenceFilter : BuffaloExpression -> Maybe Tree
sentenceFilter expr =
    case expr.semantics of
        S sentence ->
            Just expr.tree

        _ ->
            Nothing


buffalo : Int -> ExpressionFilter -> List Tree
buffalo num filter =
    buffaloParser num |> toRenderTrees filter


buffaloParser : Int -> List BuffaloExpression
buffaloParser num =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN, buffaloCity, buffaloNP, buffaloIntrVerb, buffaloTrVerb ]

        _ ->
            let
                nextDirect =
                    buffaloParser (num - 1)
                        |> List.map (expressionApplication 1)
                        |> List.concat

                skipNext =
                    buffaloParser (num - 2)
                        |> List.map (expressionApplication 2)
                        |> List.concat
            in
            List.concat [ nextDirect, skipNext ]
