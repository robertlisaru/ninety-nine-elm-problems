module RleEncode exposing (test)

import List


type RleCode a
    = Run Int a
    | Single a


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
    let
        firstSequence =
            getFirstSequence list

        remaining =
            list |> List.drop (List.length firstSequence)
    in
    case list of
        [] ->
            []

        _ ->
            firstSequence :: pack remaining


encodePacked : List (List a) -> List (RleCode a)
encodePacked packedList =
    case packedList of
        firstSubList :: rest ->
            case firstSubList of
                firstElement :: _ ->
                    case List.length firstSubList of
                        1 ->
                            Single firstElement :: encodePacked rest

                        _ ->
                            Run (List.length firstSubList) firstElement :: encodePacked rest

                [] ->
                    encodePacked rest

        [] ->
            []


rleEncode : List a -> List (RleCode a)
rleEncode list =
    list |> pack |> encodePacked


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
            , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
            , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
            , rleEncode [ 1 ] == [ Single 1 ]
            , rleEncode [] == []
            , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
            , rleEncode [ "aab", "b", "b", "aa" ]
                == [ Single "aab", Run 2 "b", Single "aa" ]
            ]
