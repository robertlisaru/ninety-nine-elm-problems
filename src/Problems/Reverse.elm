module Problems.Reverse exposing (test)


myReverse : List a -> List a
myReverse list =
    list |> List.foldl (::) []


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ myReverse [ 1, 2, 3, 4 ] == [ 4, 3, 2, 1 ]
            , myReverse [ 2, 1 ] == [ 1, 2 ]
            , myReverse [ 1 ] == [ 1 ]
            , myReverse [] == []
            , myReverse [ 'a', 'b', 'c' ] == [ 'c', 'b', 'a' ]
            ]
