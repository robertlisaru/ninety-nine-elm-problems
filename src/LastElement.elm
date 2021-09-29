module LastElement exposing (test)

import List


last : List a -> Maybe a
last xs =
    xs
        |> List.reverse
        |> List.head


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ last (List.range 1 4) == Just 4
            , last [ 1 ] == Just 1
            , last [] == Nothing
            , last [ 'a', 'b', 'c' ] == Just 'c'
            ]
