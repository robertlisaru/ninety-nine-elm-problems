module Problems.Duplicate exposing (test)

import List


duplicate : List a -> List a
duplicate list =
    list |> List.concatMap (\a -> [ a, a ])


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ duplicate [ 1, 2, 3, 5, 8, 8 ] == [ 1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8 ]
            , duplicate [] == []
            , duplicate [ 1 ] == [ 1, 1 ]
            , duplicate [ "1", "2", "5" ] == [ "1", "1", "2", "2", "5", "5" ]
            ]
