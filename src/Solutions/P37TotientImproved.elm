module Solutions.P37TotientImproved exposing (totient)

import List


totient : Int -> Int
totient m =
    let
        formula ( p_, m_ ) =
            (p_ - 1) * p_ ^ (m_ - 1)
    in
    if m < 1 then
        0

    else
        m |> primeFactorsM |> List.map formula |> List.product


primeFactorsM : Int -> List ( Int, Int )
primeFactorsM m =
    toTuples <| primeFactors m


primeFactors : Int -> List Int
primeFactors m =
    if m < 2 then
        []

    else
        let
            divides m_ x =
                (m_ |> modBy x) == 0

            prime =
                List.range 2 m
                    |> dropWhile (not << divides m)
                    |> List.head
                    |> Maybe.withDefault 0
        in
        prime :: (primeFactors <| m // prime)


toTuples : List a -> List ( a, Int )
toTuples factors =
    case factors of
        [] ->
            []

        x :: _ ->
            ( x, List.length (takeWhile ((==) x) factors) )
                :: toTuples (dropWhile ((==) x) factors)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile shouldDrop list =
    case list of
        [] ->
            []

        first :: rest ->
            if shouldDrop first then
                dropWhile shouldDrop rest

            else
                list


takeWhile : (a -> Bool) -> List a -> List a
takeWhile shouldTake list =
    case list of
        [] ->
            []

        first :: rest ->
            if shouldTake first then
                first :: takeWhile shouldTake rest

            else
                []
