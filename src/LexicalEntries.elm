module LexicalEntries exposing (buffaloMammalGroup)


type Representation
    = Verbose
    | PredicateLogic
    | Emoji


buffaloMammalGroup : Representation -> String
buffaloMammalGroup rep =
    case rep of
        Verbose ->
            "[the group of mammals within the subfamily Bovinae]"

        PredicateLogic ->
            "[Ɩx[bovinae'(x)]]"

        Emoji ->
            "[the 🐂]"


buffaloCityPredicate : Representation -> String
buffaloCityPredicate rep =
    case rep of
        Verbose ->
            " is from Buffalo"

        PredicateLogic ->
            "from-buffalo'(x)"

        Emoji ->
            " is from 🗽 "


buffaloVerbPredicate : Representation -> String
buffaloVerbPredicate rep =
    case rep of
        Verbose ->
            " bullies "

        PredicateLogic ->
            "bullies'"

        Emoji ->
            " 👺 "
