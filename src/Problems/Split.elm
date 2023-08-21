module Problems.Split exposing (test)

import List


split : List a -> Int -> ( List a, List a )
split list count =
    ( list |> List.take count, list |> List.drop count )


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ split (List.range 1 5) 0 == ( [], [ 1, 2, 3, 4, 5 ] )
            , split (List.range 1 5) 2 == ( [ 1, 2 ], [ 3, 4, 5 ] )
            , split (List.range 1 5) 3 == ( [ 1, 2, 3 ], [ 4, 5 ] )
            , split (List.range 1 5) 4 == ( [ 1, 2, 3, 4 ], [ 5 ] )
            , split (List.range 1 5) 5 == ( [ 1, 2, 3, 4, 5 ], [] )
            , split (List.range 1 5) 6 == ( [ 1, 2, 3, 4, 5 ], [] )
            , split (List.range 1 5) -1 == ( [], [ 1, 2, 3, 4, 5 ] )
            , split [ "aab", "b", "c", "aa" ] 2 == ( [ "aab", "b" ], [ "c", "aa" ] )
            ]
