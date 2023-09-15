module Problems.P2Penultimate exposing (penultimate)

import List


penultimate : List a -> Maybe a
penultimate list =
    list
        |> List.reverse
        |> List.tail
        |> Maybe.withDefault []
        |> List.head
