module RandomUtils exposing (duplicateSequences, nestedListGenerator, randomList, sometimesPalindrome)

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
            Random.int 1 10 |> Random.map Elem
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

        palindrome =
            sometimesTrue
                |> Random.andThen
                    (\isTrue ->
                        if isTrue then
                            evenLengthPalindrome

                        else
                            oddLengthPalindrome
                    )

        evenLengthPalindrome =
            randomList 4 |> Random.andThen (\list -> Random.constant ((list |> List.reverse) ++ list))

        oddLengthPalindrome =
            randomList 4 |> Random.andThen (\list -> Random.constant ((list |> List.reverse) ++ (list |> List.drop 1)))
    in
    sometimesTrue
        |> Random.andThen
            (\isTrue ->
                if isTrue then
                    palindrome

                else
                    randomList 8
            )


duplicateSequences : Random.Generator (List Int)
duplicateSequences =
    let
        getSequence sequenceLength =
            Random.int 1 10 |> Random.map (List.repeat sequenceLength)

        maxSequenceLength =
            3

        sequenceOfRandomLength =
            Random.int 0 maxSequenceLength |> Random.andThen getSequence

        nrOfSequences =
            5
    in
    Random.list nrOfSequences sequenceOfRandomLength |> Random.map List.concat


randomList : Int -> Random.Generator (List Int)
randomList maxLength =
    Random.int 0 maxLength
        |> Random.andThen (\n -> Random.list n (Random.int 1 10))
