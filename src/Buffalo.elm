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
    }



-- Function application builds up tree and parses expressions


buffaloNP : BuffaloExpression
buffaloNP =
    { surface = "the group of mammals within the subfamily Bovinae"
    , semantics = NP "the group of mammals within the subfamily Bovinae"
    }



-- noun : String -> (String -> String) -> String
-- noun predicate adj =
--     \px -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x"


buffaloN : BuffaloExpression
buffaloN =
    { surface = "\\x[x is a member of the group of mammals within the subfamily Bovinae]"
    , semantics = N (\p -> "the x s.t. is a member of the group of mammals within the subfamily Bovinae and " ++ p "x")
    }


buffaloCity : BuffaloExpression
buffaloCity =
    { surface = "\\P[the x such that x is from Buffalo and P(x)]"
    , semantics = AdjP (\x -> "x is from Buffalo")
    }



-- expressionApplication : BuffaloExpression -> List BuffaloExpression
-- expressionApplication expr =
--   let
--       surface = "buffalo " ++ expr.surface
--       semantics =
--         case expr of
--           NP individual ->
--             expr -- TODO
--           N predicate ->
--             NP (predicate)
--           AdjP predicate ->
--   in
--     BuffaloExpression surface semantics


toRenderTree : BuffaloExpression -> Tree
toRenderTree ({ semantics, surface } as buffaloExpr) =
    case semantics of
        NP individual ->
            Node ( RenderedNode "NP" surface, [] )

        N predicate ->
            Node ( RenderedNode "N" surface, [] )

        AdjP predicate ->
            Node ( RenderedNode "AdjP" surface, [] )


toRenderTrees : List BuffaloExpression -> List Tree
toRenderTrees buffaloExprs =
    List.map toRenderTree buffaloExprs


buffalo : Int -> List Tree
buffalo num =
    case num of
        0 ->
            []

        1 ->
            buffaloParser num |> toRenderTrees

        2 ->
            -- List.map expressionApplication (buffalo 1)
            -- |> toRenderTrees
            [ Node
                ( RenderedNode "NP" "the meaning of this expression"
                , [ toRenderTree buffaloCity
                  , toRenderTree buffaloN
                  ]
                )
            ]

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
