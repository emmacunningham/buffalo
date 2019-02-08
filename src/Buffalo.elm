module Buffalo exposing (BuffaloExpression, SyntacticCategory(..))


type SyntacticCategory
    = N
    | NP
    | TransitiveVerb
    | IntransitiveVerb
    | AdjP


type SemanticValue
    = CommonNoun (String -> String)
    | DefiniteNP String
    | CityAdjP (String -> String)


type alias BuffaloExpression =
    { surface : String
    , semantics : SemanticValue
    , syntax : SyntacticCategory
    }


buffaloNP : BuffaloExpression
buffaloNP =
    { surface = "buffalo"
    , semantics = DefiniteNP "the group of mammals within the subfamily Bovinae"
    , syntax = NP
    }


buffaloN : BuffaloExpression
buffaloN =
    { surface = "buffalo"
    , semantics = CommonNoun (\x -> "∃y[" ++ x ++ "is a member of the group of mammals within the subfamily Bovinae]")
    , syntax = NP
    }


buffaloCity : BuffaloExpression
buffaloCity =
    { surface = "buffalo"
    , semantics = CityAdjP (\p -> "the x such that x is from Buffalo and " ++ p ++ "(x)")
    , syntax = NP
    }


buffalo : Int -> List BuffaloExpression
buffalo num =
    case num of
        0 ->
            []

        1 ->
            [ buffaloN, buffaloCity, buffaloNP ]

        _ ->
            []
