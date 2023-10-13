module ProblemText exposing (requirement)

import Css exposing (margin, px)
import Html.Styled exposing (Html, a, code, div, h4, p, text)
import Html.Styled.Attributes exposing (css, href)
import HtmlUtils exposing (viewCode)
import Styles exposing (codeStyles, problemTitleStyles)


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

        22 ->
            p []
                [ text "Create a list containing all integers within a given range, inclusively, allow for reverse order." ]

        23 ->
            p []
                [ text "Extract a given number of randomly selected elements from a list." ]

        24 ->
            p []
                [ text "Draw n different random numbers from a range of numbers." ]

        26 ->
            div []
                [ p []
                    [ text "In how many ways you choose a committee of 3 from a group of 12 people? The "
                    , a [ href "https://www.mathwords.com/c/combination_formula.htm" ]
                        [ text "combination formula" ]
                    , text " tells us C(12,3) = 220 possibilities."
                    ]
                , p [] [ text "Write a function to generate all combinations of a list." ]
                ]

        28 ->
            div []
                [ h4 [ css (problemTitleStyles ++ [ margin (px 0) ]) ] [ text "a)" ]
                , p []
                    [ code [ css codeStyles ] [ text "List.sort" ]
                    , text " will sort a list from lowest to highest."
                    ]
                , viewCode "List.sort [3, 5, 1, 10 -2] == [-2, 1, 3, 5, 10]"
                , p []
                    [ text "When you need other sort logic pass a function to "
                    , code [ css codeStyles ] [ text "List.sortBy" ]
                    ]
                , p []
                    [ text "Sort a list of lists by the length of the lists. The order of sublists of the same size is undefined."
                    ]
                , h4 [ css (problemTitleStyles ++ [ margin (px 0) ]) ] [ text "b)" ]
                , p []
                    [ text "Sort a list according to the frequency of the sublist length. Place lists with rare lengths first, those with more frequent lengths come later. If the frequency of two or more sublists are equal then any order is acceptable. "
                    ]
                ]

        31 ->
            p [] [ text "Determine whether a given integer number is prime." ]

        _ ->
            p [] [ text "Problem requirement here" ]
