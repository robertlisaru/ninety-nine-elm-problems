module Solutions.P15RepeatElements exposing (repeatElements)

import List


repeatElements : Int -> List a -> List a
repeatElements count list =
    list |> List.concatMap (\a -> List.repeat count a)
