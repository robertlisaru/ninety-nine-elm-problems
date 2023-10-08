module Solutions.P17Split exposing (split)

import List


split : Int -> List a -> ( List a, List a )
split count list =
    ( list |> List.take count, list |> List.drop count )
