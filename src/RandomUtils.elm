module RandomUtils exposing (nestedListGenerator, sometimesConsecutiveDuplicates, sometimesPalindrome)

import Random
import Solutions.P7FlattenNestedList exposing (NestedList(..))


nestedListGenerator : Float -> Random.Generator (NestedList Int)
nestedListGenerator initialSubListProbability =
    let
        clampedProbability =
            if initialSubListProbability < 0 then
                0.0

            else if initialSubListProbability > 1.0 then
                1.0

            else
                initialSubListProbability

        maxSubListLength =
            3

        sometimesTrue =
            Random.weighted ( clampedProbability, True ) [ ( 1 - clampedProbability, False ) ]

        randomLength =
            Random.int 1 maxSubListLength

        randomSubList =
            randomLength
                |> Random.andThen randomSubListOfLength
                |> Random.map SubList

        randomSubListOfLength length =
            Random.list length (Random.lazy (\_ -> nestedListGenerator (clampedProbability / 2)))

        randomElem =
            Random.int 1 100 |> Random.map Elem
    in
    sometimesTrue
        |> Random.andThen
            (\isTrue ->
                if isTrue then
                    randomSubList

                else
                    randomElem
            )


sometimesPalindrome : Random.Generator (List Int)
sometimesPalindrome =
    let
        sometimesTrue =
            Random.weighted ( 0.5, True ) [ ( 1 - 0.5, False ) ]

        randomList maxLength =
            Random.int 0 maxLength
                |> Random.andThen (\n -> Random.list n (Random.int 1 10))

        palindrome =
            randomList 5 |> Random.andThen (\list -> Random.constant ((list |> List.reverse) ++ list))
    in
    sometimesTrue
        |> Random.andThen
            (\isTrue ->
                if isTrue then
                    palindrome

                else
                    randomList 10
            )


sometimesConsecutiveDuplicates : Random.Generator (List Int)
sometimesConsecutiveDuplicates =
    let
        sometimesTrue =
            Random.weighted ( 0.5, True ) [ ( 1 - 0.5, False ) ]

        randomList =
            Random.int 0 5
                |> Random.andThen (\n -> Random.list n (Random.int 1 10))

        duplicates length =
            Random.int 1 10 |> Random.map (List.repeat length)

        sometimesDuplicates =
            sometimesTrue
                |> Random.andThen
                    (\isTrue ->
                        if isTrue then
                            Random.int 0 5 |> Random.andThen duplicates

                        else
                            randomList
                    )
    in
    Random.list 3 sometimesDuplicates |> Random.map List.concat
