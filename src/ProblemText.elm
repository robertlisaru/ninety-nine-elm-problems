module ProblemText exposing (requirement)

import Html.Styled exposing (Html, code, p, text)
import Html.Styled.Attributes exposing (css)
import HtmlUtils exposing (viewCode)
import Styles exposing (codeStyles)


requirement : Int -> Html msg
requirement problemNumber =
    case problemNumber of
        1 ->
            p []
                [ text "Write a function "
                , code [ css codeStyles ] [ text "last" ]
                , text " that returns the last element of a list. An empty list doesn't have a last element, therefore "
                , code [ css codeStyles ] [ text "last" ]
                , text " must return a "
                , code [ css codeStyles ] [ text "Maybe" ]
                , text "."
                ]

        2 ->
            p []
                [ text "Implement the function "
                , code [ css codeStyles ] [ text "penultimate" ]
                , text " to find the next to last element of a list."
                ]

        4 ->
            p []
                [ text "Elm provides the function "
                , code [ css codeStyles ] [ text "List.length" ]
                , text ". See if you can implement it yourself."
                ]

        5 ->
            p []
                [ text "Elm provides the function "
                , code [ css codeStyles ] [ text "List.reverse" ]
                , text " to reverse the order of elements in a list. See if you can implement it."
                ]

        6 ->
            p []
                [ text "Determine if a list is a palindrome, that is, the list is identical when read forward or backward." ]

        8 ->
            p []
                [ text "Write a function to remove consecutive duplicates of list elements." ]

        9 ->
            p []
                [ text "Convert a list to a list of lists where repeated elements of the source list are packed into sublists. Elements that are not repeated should be placed in a one element sublist." ]

        11 ->
            p []
                [ text "Write a function to run length encode a list, but instead of using a tuple as in problem 10, use a union data type."
                , viewCode "type RleCode a = Run Int a | Single a"
                ]

        12 ->
            p []
                [ text "Decompress the run-length encoded list generated in Problem 11." ]

        14 ->
            p []
                [ text "Duplicate each element of a list." ]

        15 ->
            p []
                [ text "Repeat each element of a list a given number of times." ]

        _ ->
            p [] [ text "Problem requirement here" ]
