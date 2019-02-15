module Buffalo exposing (buffalo)

import Expression exposing (ExpressionFilter(..), Representation(..))
import LexicalEntries exposing (..)
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


buffaloNP : Representation -> BuffaloExpression
buffaloNP rep =
    { semantics = NP (buffaloMammalGroup rep)
    , tree = TerminalNode (RenderedNode "NP" (buffaloMammalGroup rep))
    }


buffaloN : Representation -> BuffaloExpression
buffaloN rep =
    { semantics = N (\p -> "the x s.t. is a member of " ++ buffaloMammalGroup rep ++ " and " ++ p "x")
    , tree = TerminalNode (RenderedNode "N" ("\\x[x is a member of " ++ buffaloMammalGroup rep ++ "]"))
    }


buffaloCity : Representation -> BuffaloExpression
buffaloCity rep =
    { semantics = AdjP (\x -> "[" ++ x ++ buffaloCityPredicate rep ++ "]")
    , tree = TerminalNode (RenderedNode "AdjP" ("\\x[x" ++ buffaloCityPredicate rep ++ "]"))
    }


buffaloIntrVerb : Representation -> BuffaloExpression
buffaloIntrVerb rep =
    { semantics = VP (\x -> "[" ++ x ++ buffaloVerbPredicate rep ++ "(someone)]")
    , tree = TerminalNode (RenderedNode "VP" ("\\x[x" ++ buffaloVerbPredicate rep ++ "(someone)]"))
    }


buffaloTrVerb : Representation -> BuffaloExpression
buffaloTrVerb rep =
    { semantics = TrVerb (\o s -> "[" ++ s ++ buffaloVerbPredicate rep ++ o ++ "]")
    , tree = TerminalNode (RenderedNode "Verb" ("\\o s[s" ++ buffaloVerbPredicate rep ++ "o]"))
    }


attachRC : Individual -> (Individual -> TruthStatement) -> Individual
attachRC individual predicate =
    predicate (individual ++ " that ")


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


expressionApplication : Int -> Representation -> BuffaloExpression -> List BuffaloExpression
expressionApplication takeNext rep ({ tree, semantics } as expr) =
    let
        possibleExprs =
            buffaloParser takeNext rep
    in
    case semantics of
        N predicate ->
            List.filterMap (applyN tree predicate) possibleExprs

        VP predicate ->
            let
                sentence =
                    List.filterMap (applyVP tree predicate) possibleExprs

                relativeClause =
                    List.filterMap (transformVP tree predicate) possibleExprs
            in
            List.concat [ sentence, relativeClause ]

        NP individual ->
            List.filterMap (applyNP tree individual) possibleExprs

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


buffalo : Int -> ExpressionFilter -> Representation -> List Tree
buffalo num filter rep =
    buffaloParser num rep |> toRenderTrees filter


buffaloParser : Int -> Representation -> List BuffaloExpression
buffaloParser num rep =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN rep, buffaloCity rep, buffaloNP rep, buffaloIntrVerb rep, buffaloTrVerb rep ]

        _ ->
            let
                nextDirect =
                    buffaloParser (num - 1) rep
                        |> List.map (expressionApplication 1 rep)
                        |> List.concat

                skipNext =
                    buffaloParser (num - 2) rep
                        |> List.map (expressionApplication 2 rep)
                        |> List.concat
            in
            List.concat [ nextDirect, skipNext ]
