module Solutions.P10RunLengths exposing (runLengths)

import List


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
