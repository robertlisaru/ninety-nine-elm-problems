module Solutions.P33Coprimes exposing (coprimes)


coprimes : Int -> Int -> Bool
coprimes m n =
    let
        gcd a b =
            if b == 0 then
                abs a

            else
                gcd b (a |> remainderBy b)
    in
    gcd m n == 1
