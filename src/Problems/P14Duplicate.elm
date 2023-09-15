module Problems.P14Duplicate exposing (duplicate)

import List


duplicate : List a -> List a
duplicate list =
    list |> List.concatMap (\a -> [ a, a ])
