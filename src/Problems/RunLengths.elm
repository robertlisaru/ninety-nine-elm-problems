module Problems.RunLengths exposing (test)

import List



{- Run-length encode a list of list to a list of tuples. Unlike lists, tuples can mix types.
   Use tuples (n, e) to encode a list where n is the number of duplicates of the element e.
-}


runLengths : List (List a) -> List ( Int, a )
runLengths list =
    case list of
        firstSubList :: rest ->
            case firstSubList of
                firstElement :: _ ->
                    ( List.length firstSubList, firstElement ) :: runLengths rest

                [] ->
                    runLengths rest

        [] ->
            []


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
            , runLengths [ [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                == [ ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
            , runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ] ]
                == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ) ]
            , runLengths [ [ 1, 1, 1, 1 ] ]
                == [ ( 4, 1 ) ]
            , runLengths [ [ "a", "a", "a", "a" ], [ "b" ], [ "c", "c" ], [ "b" ], [ "a" ] ]
                == [ ( 4, "a" ), ( 1, "b" ), ( 2, "c" ), ( 1, "b" ), ( 1, "a" ) ]
            , runLengths [ [] ] == []
            , runLengths [ [], [ "a", "a" ] ] == [ ( 2, "a" ) ]
            , runLengths [] == []
            ]
