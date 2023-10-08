module ProblemText exposing (requirement)

import Html.Styled exposing (Html, code, div, p, text)
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

        3 ->
            p []
                [ text "Implement the function "
                , code [ css codeStyles ] [ text "elementAt" ]
                , text " to return the n-th element of a list. The index is 1-relative, that is, the first element is at index 1."
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

        7 ->
            p []
                [ text "Flatten a nested lists into a single list. Because Lists in Elm are homogeneous we need to define what a nested list is."
                , viewCode "type NestedList a = Elem a | List [NestedList a]"
                ]

        8 ->
            p []
                [ text "Write a function to remove consecutive duplicates of list elements." ]

        9 ->
            p []
                [ text "Convert a list to a list of lists where repeated elements of the source list are packed into sublists. Elements that are not repeated should be placed in a one element sublist." ]

        10 ->
            p []
                [ text "Run-length encode a list of list to a list of tuples. Unlike lists, tuples can mix types. Use tuples (n, e) to encode a list where n is the number of duplicates of the element e." ]

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

        16 ->
            p []
                [ text "Drop every nth element from a list." ]

        17 ->
            p []
                [ text "Split a list into two lists. The length of the first part is specified by the caller." ]

        18 ->
            div []
                [ p [] [ text "Extract a slice from a list." ]
                , p [] [ text "Given a list, return the elements between (inclusively) two indices. Start counting the elements with 1. Indices outside of the list bounds (i.e. negative number, or beyond the length of the list) should be clipped to the bounds of the list." ]
                ]

        19 ->
            p []
                [ text "Rotate a list "
                , code [ css codeStyles ] [ text "n" ]
                , text " places to the left (negative values will rotate to the right). Allow rotations greater than the list length."
                ]

        20 ->
            p []
                [ text "Drop the nth element from a list." ]

        21 ->
            p []
                [ text "Insert an element at a given position into a list. Treat the first position as index 1." ]

        _ ->
            p [] [ text "Problem requirement here" ]
