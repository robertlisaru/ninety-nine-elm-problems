module Solutions.P18Sublist exposing (sublist)

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
