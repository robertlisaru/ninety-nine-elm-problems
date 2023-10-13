module Solutions.P32GCD exposing (gcd)


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        abs a

    else
        gcd b (a |> remainderBy b)
