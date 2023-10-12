module Solutions.P28SortBy exposing (sortByLengthFrequency, sortByListLengths)

import List


sortByListLengths : List (List a) -> List (List a)
sortByListLengths listOfLists =
    listOfLists |> List.sortBy List.length


sortByLengthFrequency : List (List a) -> List (List a)
sortByLengthFrequency listOfLists =
    let
        lengthFrequency list =
            listOfLists
                |> List.map List.length
                |> List.filter ((==) (List.length list))
                |> List.length
    in
    listOfLists
        |> List.sortBy lengthFrequency
