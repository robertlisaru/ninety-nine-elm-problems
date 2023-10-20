module Solutions.P40Goldbach exposing (goldbach)

import List


goldbach : Int -> Maybe ( Int, Int )
goldbach n =
    let
        primes =
            primesInRange 2 n

        isPrime a =
            List.member a primes
    in
    primes
        |> List.map (\prime -> ( prime, n - prime ))
        |> List.filter (Tuple.second >> isPrime)
        |> List.head


primesInRange : Int -> Int -> List Int
primesInRange start end =
    List.range 2 end
        |> sieve
        |> List.filter (\prime -> prime >= start)


sieve : List Int -> List Int
sieve suspects =
    case suspects of
        [] ->
            []

        prime :: otherSuspects ->
            prime
                :: sieve
                    (otherSuspects
                        |> List.filter (remainderBy prime >> (/=) 0)
                    )
