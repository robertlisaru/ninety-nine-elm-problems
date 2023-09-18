module Solutions.P11RleEncode exposing (RleCode(..), rleEncode)

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
