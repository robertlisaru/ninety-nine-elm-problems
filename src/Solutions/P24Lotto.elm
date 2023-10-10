module Solutions.P24Lotto exposing (lotto)

import Array
import List
import Random exposing (andThen)


lotto : Int -> Int -> Int -> Random.Generator (List Int)
lotto n start end =
    randomSelect n (List.range start end)


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
