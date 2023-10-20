module Solutions.P41GoldbachThreshold exposing (goldbachGT)


goldbachGT : Int -> Int -> Int -> List ( Int, Int )
goldbachGT start end threshold =
    List.range start end
        |> List.filter (\n -> (n |> modBy 2) == 0)
        |> List.map goldbach
        |> removeNothings
        |> List.filter
            (\primes -> (primes |> Tuple.first) > threshold)


goldbach : Int -> Maybe ( Int, Int )
goldbach n =
    let
        primes =
            primesInRange 2 n

        isPrime a =
            List.member a primes
    in
    primes
        |> dropWhile
            (\prime -> (n - prime) |> isPrime |> not)
        |> List.head
        |> Maybe.map (\prime -> ( prime, n - prime ))


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


removeNothings : List (Maybe a) -> List a
removeNothings listMaybes =
    listMaybes
        |> List.foldr
            (\maybeA list ->
                case maybeA of
                    Just a ->
                        a :: list

                    Nothing ->
                        list
            )
            []


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
