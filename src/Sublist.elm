module Sublist exposing (test)

import List


sublist : Int -> Int -> List a -> List a
sublist start end list =
    let
        dropCount =
            if start < 2 then
                0

            else
                start - 1

        takeCount =
            if end > List.length list then
                List.length list

            else
                end - dropCount
    in
    list |> List.drop dropCount |> List.take takeCount


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ sublist 3 7 (List.range 1 10) == List.range 3 7
            , sublist 2 100 [ 'a', 'b', 'c' ] == [ 'b', 'c' ]
            , sublist -1 2 (List.range 1 100) == [ 1, 2 ]
            , sublist -3 -2 [ -3, -2, -1, 0, 1, 2, 3 ] == []
            , sublist 5 3 [ "indices", " are", "inverted" ] == []
            , sublist 0 1 (List.range 1 10) == [ 1 ]
            , sublist -7 -3 (List.range 1 10) == []
            ]
