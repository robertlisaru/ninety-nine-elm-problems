module Solutions.P5Reverse exposing (myReverse)


myReverse : List a -> List a
myReverse list =
    list |> List.foldl (::) []
