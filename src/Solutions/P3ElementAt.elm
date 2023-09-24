module Solutions.P3ElementAt exposing (elementAt)

import List


elementAt : Int -> List a -> Maybe a
elementAt n list =
    if n < 1 then
        Nothing

    else
        list
            |> List.drop (n - 1)
            |> List.head
