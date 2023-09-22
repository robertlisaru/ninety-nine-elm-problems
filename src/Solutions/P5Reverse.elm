module Solutions.P5Reverse exposing (myReverse)

import List



{-
   Without using List.reverse
-}


myReverse : List a -> List a
myReverse list =
    list |> List.foldl (::) []
