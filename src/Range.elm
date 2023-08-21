module Range exposing (test)

import List


range : Int -> Int -> List Int
range start end =
    if start < end then
        List.range start end

    else
        List.range end start |> List.reverse


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ range 1 5 == [ 1, 2, 3, 4, 5 ]
            , range 0 5 == [ 0, 1, 2, 3, 4, 5 ]
            , range -1 5 == [ -1, 0, 1, 2, 3, 4, 5 ]
            , range 5 -1 == [ 5, 4, 3, 2, 1, 0, -1 ]
            , range 5 5 == [ 5 ]
            , List.length (range 1 999) == 999
            ]
