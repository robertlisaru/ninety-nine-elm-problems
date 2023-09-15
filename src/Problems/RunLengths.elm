module Problems.RunLengths exposing (runLengths)

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
