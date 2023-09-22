module Solutions.P4CountElements exposing (countElements)

import List



{-
   Without using List.length
-}


countElements : List a -> Int
countElements list =
    list |> List.map (always 1) |> List.sum
