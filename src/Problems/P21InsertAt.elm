module Problems.P21InsertAt exposing (insertAt)

import List


insertAt : Int -> a -> List a -> List a
insertAt n v list =
    (list |> List.take (n - 1)) ++ (v :: (list |> List.drop (n - 1)))
