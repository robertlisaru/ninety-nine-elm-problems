module Solutions.P34Totient exposing (totient)


totient : Int -> Int
totient m =
    List.range 1 m
        |> List.filter (coprimes m)
        |> List.length


coprimes : Int -> Int -> Bool
coprimes x y =
    gcd x y == 1


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        abs a

    else
        gcd b (a |> remainderBy b)
