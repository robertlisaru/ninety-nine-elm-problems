module ProblemText exposing (requirement)

import Html.Styled exposing (Html, a, code, div, h4, li, ol, p, pre, text)
import Html.Styled.Attributes exposing (css, href)
import HtmlUtils exposing (viewCode)
import Styles exposing (codeBlockStyles, codeLineStyles, problemSubTitleStyles)


requirement : Int -> Html msg
requirement problemNumber =
    case problemNumber of
        1 ->
            p []
                [ text "Write a function "
                , code [ css codeLineStyles ] [ text "last" ]
                , text " that returns the last element of a list. An empty list doesn't have a last element, therefore "
                , code [ css codeLineStyles ] [ text "last" ]
                , text " must return a "
                , code [ css codeLineStyles ] [ text "Maybe" ]
                , text "."
                ]

        2 ->
            p []
                [ text "Implement the function "
                , code [ css codeLineStyles ] [ text "penultimate" ]
                , text " to find the next to last element of a list."
                ]

        3 ->
            p []
                [ text "Implement the function "
                , code [ css codeLineStyles ] [ text "elementAt" ]
                , text " to return the n-th element of a list. The index is 1-relative, that is, the first element is at index 1."
                ]

        4 ->
            p []
                [ text "Elm provides the function "
                , code [ css codeLineStyles ] [ text "List.length" ]
                , text ". See if you can implement it yourself."
                ]

        5 ->
            p []
                [ text "Elm provides the function "
                , code [ css codeLineStyles ] [ text "List.reverse" ]
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
                , code [ css codeLineStyles ] [ text "n" ]
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
                [ h4 [ css problemSubTitleStyles ] [ text "a)" ]
                , p []
                    [ code [ css codeLineStyles ] [ text "List.sort" ]
                    , text " will sort a list from lowest to highest."
                    ]
                , viewCode "List.sort [3, 5, 1, 10 -2] == [-2, 1, 3, 5, 10]"
                , p []
                    [ text "When you need other sort logic pass a function to "
                    , code [ css codeLineStyles ] [ text "List.sortBy" ]
                    ]
                , p []
                    [ text "Sort a list of lists by the length of the lists. The order of sublists of the same size is undefined."
                    ]
                , h4 [ css problemSubTitleStyles ] [ text "b)" ]
                , p []
                    [ text "Sort a list according to the frequency of the sublist length. Place lists with rare lengths first, those with more frequent lengths come later. If the frequency of two or more sublists are equal then any order is acceptable. "
                    ]
                ]

        31 ->
            p [] [ text "Determine whether a given integer number is prime." ]

        32 ->
            div []
                [ p []
                    [ text "Determine the greatest common divisor of two positive integer numbers. Use "
                    , a [ href "https://en.wikipedia.org/wiki/Euclidean_algorithm" ] [ text "Euclid's algorithm" ]
                    , text " which recurses over the following steps:"
                    ]
                , p []
                    [ ol []
                        [ li [] [ text "Given two numbers, a and b, divide a by b" ]
                        , li [] [ text "If the remainder of the division is 0, the numerator is the GCD" ]
                        , li [] [ text "Else divide the demoninator by the remainder and return to step 2" ]
                        ]
                    ]
                ]

        33 ->
            p [] [ text "Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1." ]

        34 ->
            div []
                [ p []
                    [ text "Calculate Euler's totient function "
                    , code [ css codeLineStyles ] [ text "phi(m)" ]
                    , text "."
                    ]
                , p []
                    [ text "Euler's totient function "
                    , code [ css codeLineStyles ] [ text "phi(m)" ]
                    , text " is defined as the number of positive integers "
                    , code [ css codeLineStyles ] [ text "r (1 <= r < m)" ]
                    , text " that are coprime with "
                    , code [ css codeLineStyles ] [ text "m" ]
                    , text "."
                    ]
                , p [] [ text "Note the special case: ", code [ css codeLineStyles ] [ text "totient 1 == 1" ] ]
                ]

        35 ->
            p [] [ text "Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order." ]

        36 ->
            p [] [ text "Construct a list containing the prime factors and their multiplicity for a given integer." ]

        37 ->
            div []
                [ p [] [ text "Calculate Euler's totient function phi(m) (improved)." ]
                , p []
                    [ text "See "
                    , a [ href "#34" ] [ text "problem 34" ]
                    , text " for the definition of Euler's totient function. If the list of the prime factors of a number "
                    , code [ css codeLineStyles ] [ text "m" ]
                    , text " is known in the form of "
                    , a [ href "#36" ] [ text "problem 36" ]
                    , text " then the function "
                    , code [ css codeLineStyles ] [ text "phi(m)" ]
                    , text " can be efficiently calculated as follows:"
                    ]
                , pre [ css codeBlockStyles ]
                    [ code [] [ text """Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime
factors and their multiplicities) of a given number m. 
Then phi(m) can be calculated with the following formula:

phi(m) = ((p1 - 1) * p1 ^ (m1 - 1)) * 
         ((p2 - 1) * p2 ^ (m2 - 1)) * 
         ((p3 - 1) * p3 ^ (m3 - 1)) * ...""" ] ]
                ]

        38 ->
            p []
                [ text "The application below measures the time to calculate totients using the algorithms from "
                , a [ href "#34" ] [ text "problem 34" ]
                , text " and "
                , a [ href "#37" ] [ text "problem 37" ]
                , text "."
                ]

        39 ->
            p []
                [ text "Construct a list of all prime numbers within a range of integers." ]

        40 ->
            div []
                [ p []
                    [ a [ href "https://en.wikipedia.org/wiki/Goldbach%27s_conjecture" ]
                        [ text "Goldbach's conjecture" ]
                    , text " says that every positive even integer greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It has been numerically confirmed up to very large numbers but remains unproven."
                    ]
                , p [] [ text "Return two prime numbers that sum up to a given even integer." ]
                ]

        41 ->
            p [] [ text "Find all even numbers within a range which can only be represented by the sum of two prime numbers that are both greater than a specified threshold." ]

        46 ->
            div []
                [ p [] [ text "Define functions to provide the logical binary functions and, or, nand, nor, xor, implies, and equivalent." ]
                , p [] [ viewCode """-- True if and only if a and b are true
and : Bool -> Bool -> Bool

-- True if either a or b are true
or : Bool -> Bool -> Bool

-- True either a or b are false
nand : Bool -> Bool -> Bool

-- True if and only if a and b are false
nor : Bool -> Bool -> Bool

-- True if a or b is true, but not if both are true
xor : Bool -> Bool -> Bool

-- True if a is false or b is true 
implies : Bool -> Bool -> Bool

-- True if both a and b are true, or both a and b ar false
equivalent : Bool -> Bool -> Bool""" ]
                , p [] [ text "Define a function to show a truth table for a logical expression." ]
                , p [] [ viewCode "truthTable : (Bool -> Bool -> Bool) -> List (Bool, Bool, Bool)" ]
                ]

        47 ->
            p [] [ text "Create custom infix operators for the logical functions from Problem 46." ]

        49 ->
            p []
                [ text "The "
                , a [ href "https://mathworld.wolfram.com/GrayCode.html" ] [ text "Gray Code" ]
                , text " is a binary numbering system used in error correction system. It can be generated for different bit sizes using a reflecting pattern. To generate a two-bit code, take the single bit Gray code 0, 1. Write it forwards, then backwards: 0, 1, 1, 0. Prepend 0s to the first half and 1s to the second half: 00, 01, 11, 10. To generate a 3-bit code, write 00, 01, 11, 10, 10, 11, 01, 00 to obtain: 000, 001, 011, 010, 110, 111, 101, 100."
                ]

        50 ->
            p [] [ text "Huffman coding uses variable bit length codes to efficiently encode data by giving the frequently found values short codes and rarely used values longer codes. The first step in Huffman encoding is to determine the frequency of every value in the input data. " ]

        _ ->
            p [] [ text "Problem not yet implemented." ]
