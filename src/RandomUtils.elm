module RandomUtils exposing (nestedListGenerator)

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
