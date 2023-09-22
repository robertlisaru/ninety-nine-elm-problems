module Solutions.P8NoDupes exposing (noDupes)

import List


noDupes : List a -> List a
noDupes list =
    case list of
        first :: rest ->
            if List.head rest == Just first then
                noDupes rest

            else
                first :: noDupes rest

        _ ->
            list
