module Solutions.P34Totient exposing (totient)


totient : Int -> Int
totient m =
    let
        coprimes x y =
            let
                gcd a b =
                    if b == 0 then
                        abs a

                    else
                        gcd b (a |> remainderBy b)
            in
            gcd x y == 1
    in
    List.range 1 m
        |> List.filter (coprimes m)
        |> List.length
