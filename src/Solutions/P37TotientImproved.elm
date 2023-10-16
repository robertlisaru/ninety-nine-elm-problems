module Solutions.P37TotientImproved exposing (totient)

import List


totient : Int -> Int
totient m =
    let
        formula ( prime, multiplicity ) =
            (prime - 1) * prime ^ (multiplicity - 1)
    in
    if m < 1 then
        0

    else
        m |> primeFactors |> toTuples |> List.map formula |> List.product


primeFactors : Int -> List Int
primeFactors m =
    if m < 2 then
        []

    else
        let
            isDivisibleBy x =
                (m |> modBy x) == 0

            prime =
                List.range 2 m
                    |> dropWhile (not << isDivisibleBy)
                    |> List.head
                    |> Maybe.withDefault 1
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
