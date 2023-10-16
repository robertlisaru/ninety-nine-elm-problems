module Solutions.P39PrimesInRange exposing (primesInRange)

import List


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
