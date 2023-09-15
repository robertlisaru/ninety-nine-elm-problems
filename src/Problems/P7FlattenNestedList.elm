module Problems.P7FlattenNestedList exposing (NestedList(..), flatten)

import List


type NestedList a
    = Elem a
    | SubList (List (NestedList a))


flatten : NestedList a -> List a
flatten nl =
    case nl of
        Elem a ->
            [ a ]

        SubList list ->
            list |> List.map flatten |> List.concat
