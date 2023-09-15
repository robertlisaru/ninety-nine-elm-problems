module Problems.P20DropAt exposing (dropAt)

import List


dropAt : Int -> List a -> List a
dropAt n list =
    (list |> List.take (n - 1)) ++ (list |> List.drop n)
