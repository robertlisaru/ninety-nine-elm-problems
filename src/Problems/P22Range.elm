module Problems.P22Range exposing (range)

import List


range : Int -> Int -> List Int
range start end =
    if start < end then
        List.range start end

    else
        List.range end start |> List.reverse
