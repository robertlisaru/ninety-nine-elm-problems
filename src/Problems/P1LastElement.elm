module Problems.P1LastElement exposing (last)

import List


last : List a -> Maybe a
last xs =
    xs
        |> List.reverse
        |> List.head
