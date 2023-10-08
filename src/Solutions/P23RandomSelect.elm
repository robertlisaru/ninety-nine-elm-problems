module Solutions.P23RandomSelect exposing (randomSelect)

import Array
import List
import Random exposing (andThen)


randomSelect : Int -> List a -> Random.Generator (List a)
randomSelect n list =
    let
        dropAt position elements =
            (elements |> List.take (position - 1))
                ++ (elements |> List.drop position)

        elementAt position elements =
            elements |> Array.fromList |> Array.get (position - 1)

        getRandomPosition =
            Random.int 1 (List.length list)

        consTheElementFrom position otherElements =
            elementAt position list
                |> Maybe.map (\element -> element :: otherElements)
                |> Maybe.withDefault otherElements

        recursiveResultExcluding position =
            Random.lazy
                (\_ ->
                    randomSelect (n - 1) (list |> dropAt position)
                )

        consRecursively randomPosition =
            recursiveResultExcluding randomPosition
                |> Random.map (consTheElementFrom randomPosition)
    in
    if n < 1 || list == [] then
        Random.constant []

    else
        getRandomPosition |> andThen consRecursively
