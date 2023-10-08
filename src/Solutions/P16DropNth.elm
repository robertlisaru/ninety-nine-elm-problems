module Solutions.P16DropNth exposing (dropNth)

import List


removeNothings : List (Maybe a) -> List a
removeNothings listMaybes =
    listMaybes
        |> List.foldr
            (\maybeA list ->
                case maybeA of
                    Just a ->
                        a :: list

                    Nothing ->
                        list
            )
            []


dropNth : Int -> List a -> List a
dropNth n list =
    if n < 1 then
        list

    else if n == 1 then
        []

    else
        list
            |> List.indexedMap
                (\index a ->
                    if ((index + 1) |> modBy n) == 0 then
                        Nothing

                    else
                        Just a
                )
            |> removeNothings
