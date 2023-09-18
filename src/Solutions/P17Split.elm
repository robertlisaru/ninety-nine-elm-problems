module Solutions.P17Split exposing (split)

import List


split : List a -> Int -> ( List a, List a )
split list count =
    ( list |> List.take count, list |> List.drop count )
