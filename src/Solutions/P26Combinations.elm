module Solutions.P26Combinations exposing (combinations)

import List


combinations : Int -> List a -> List (List a)
combinations n list =
    if n < 1 then
        [ [] ]

    else
        case list of
            firstElement :: otherElements ->
                (combinations (n - 1) otherElements
                    |> List.map ((::) firstElement)
                )
                    ++ combinations n otherElements

            [] ->
                []
