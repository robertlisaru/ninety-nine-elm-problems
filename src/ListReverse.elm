module ListReverse exposing (test)

import List


myReverse : List a -> List a
myReverse list =
    List.foldl (::) [] list


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
