module Solutions.P49GrayCodes exposing (grayCodes)


grayCodes : Int -> List (List Int)
grayCodes numBits =
    if numBits < 2 then
        [ [ 0 ], [ 1 ] ]

    else
        let
            previousCodes =
                grayCodes (numBits - 1)

            firstHalf =
                previousCodes |> List.map ((::) 0)

            secondHalf =
                previousCodes |> List.reverse |> List.map ((::) 1)
        in
        firstHalf ++ secondHalf
