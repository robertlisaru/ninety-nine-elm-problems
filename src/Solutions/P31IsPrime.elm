module Solutions.P31IsPrime exposing (isPrime)

import List


isPrime : Int -> Bool
isPrime n =
    (n > 1)
        && (List.range 2 (n |> toFloat |> sqrt |> floor)
                |> List.all (\d -> (n |> modBy d) /= 0)
           )
