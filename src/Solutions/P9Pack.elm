module Solutions.P9Pack exposing (pack)

import List



{- Convert a list to a list of lists where repeated elements of the source list are packed into sublists.
   Elements that are not repeated should be placed in a one element sublist.
-}


getFirstSequence : List a -> List a
getFirstSequence list =
    case list of
        first :: rest ->
            if List.head rest == Just first then
                first :: getFirstSequence rest

            else
                [ first ]

        _ ->
            list


pack : List a -> List (List a)
pack list =
    case list of
        [] ->
            []

        _ ->
            let
                firstSequence =
                    getFirstSequence list

                remaining =
                    list |> List.drop (List.length firstSequence)
            in
            firstSequence :: pack remaining
