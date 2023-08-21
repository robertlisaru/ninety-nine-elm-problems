module Problems.Penultimate exposing (test)

import List


penultimate : List a -> Maybe a
penultimate list =
    list
        |> List.reverse
        |> List.tail
        |> Maybe.withDefault []
        |> List.head


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ penultimate [ 1, 2, 3, 4 ] == Just 3
            , penultimate [ 1, 2 ] == Just 1
            , penultimate [ 1 ] == Nothing
            , penultimate [] == Nothing
            , penultimate [ "a", "b", "c" ] == Just "b"
            , penultimate [ "a" ] == Nothing
            ]
