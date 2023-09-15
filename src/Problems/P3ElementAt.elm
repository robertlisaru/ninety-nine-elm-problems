module Problems.P3ElementAt exposing (elementAt)

import List


elementAt : List a -> Int -> Maybe a
elementAt list n =
    if n < 1 then
        Nothing

    else
        list
            |> List.drop (n - 1)
            |> List.head
