module RleEncode exposing (test)

import List


type RleCode a
    = Run Int a
    | Single a


incrementRun : Maybe (RleCode a) -> Maybe (RleCode a)
incrementRun rleCode =
    case rleCode of
        Just (Run count a) ->
            Just (Run (count + 1) a)

        Just (Single a) ->
            Just (Run 2 a)

        _ ->
            Nothing


sequenceLength : RleCode a -> Int
sequenceLength rleCode =
    case rleCode of
        Run length _ ->
            length

        Single _ ->
            1


getFirstSequence : List a -> Maybe (RleCode a)
getFirstSequence list =
    case list of
        first :: rest ->
            if List.head rest == Just first then
                incrementRun (getFirstSequence rest)

            else
                Just (Single first)

        _ ->
            Nothing


rleEncode : List a -> List (RleCode a)
rleEncode list =
    case list of
        [] ->
            []

        _ ->
            case getFirstSequence list of
                Just firstSequence ->
                    let
                        remaining =
                            list |> List.drop (sequenceLength firstSequence)
                    in
                    firstSequence :: rleEncode remaining

                Nothing ->
                    []


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
