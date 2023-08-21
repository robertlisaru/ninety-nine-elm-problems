module Problems.DropNth exposing (test)

import Html exposing (a)
import Html.Attributes exposing (list)
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


dropNth : List a -> Int -> List a
dropNth list n =
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


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ dropNth [ 1, 2, 5, 5, 2, 1 ] 2 == [ 1, 5, 2 ]
            , dropNth (List.range 1 20) 3 == [ 1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20 ]
            , dropNth (List.range 1 5) 6 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) 0 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) -1 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) 1 == []
            , dropNth [ "1", "2", "3", "4", "5", "6" ] 2 == [ "1", "3", "5" ]
            ]
