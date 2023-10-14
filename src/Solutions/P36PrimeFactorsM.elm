module Solutions.P36PrimeFactorsM exposing (primeFactorsM)


primeFactorsM : Int -> List ( Int, Int )
primeFactorsM m =
    let
        uniqueFactors =
            primesInRange 2 m
                |> List.filter (\d -> (m |> remainderBy d) == 0)
    in
    uniqueFactors
        |> List.map
            (\factor -> ( factor, multiplicity m factor ))


multiplicity : Int -> Int -> Int
multiplicity m factor =
    if (m |> remainderBy factor) == 0 then
        1 + multiplicity (m // factor) factor

    else
        0


isPrime : Int -> Bool
isPrime n =
    (n > 1)
        && (List.range 2 (n |> toFloat |> sqrt |> floor)
                |> List.filter (\d -> (n |> modBy d) == 0)
                |> List.length
           )
        == 0


primesInRange : Int -> Int -> List Int
primesInRange start end =
    List.range start end |> List.filter isPrime
