module Main exposing (main)

import CountElements
import Duplicate
import ElementAt
import FlattenNestedList
import Html exposing (Html, li, ul)
import IsPalindrome
import LastElement
import NoDupes
import Pack
import Penultimate
import RepeatElements
import Reverse
import RleDecode
import RleEncode
import RunLengths


problems : List Problem
problems =
    [ { identifier = "1", title = "Last element", testFunction = LastElement.test }
    , { identifier = "2", title = "Penultimate", testFunction = Penultimate.test }
    , { identifier = "3", title = "Element at", testFunction = ElementAt.test }
    , { identifier = "4", title = "Count elements", testFunction = CountElements.test }
    , { identifier = "5", title = "Reverse", testFunction = Reverse.test }
    , { identifier = "6", title = "Is palindrome", testFunction = IsPalindrome.test }
    , { identifier = "7", title = "Flatten nested list", testFunction = FlattenNestedList.test }
    , { identifier = "8", title = "No dupes", testFunction = NoDupes.test }
    , { identifier = "9", title = "Pack", testFunction = Pack.test }
    , { identifier = "10", title = "Run lengths", testFunction = RunLengths.test }
    , { identifier = "11", title = "Run lengths encode", testFunction = RleEncode.test }
    , { identifier = "12", title = "Run lengths decode", testFunction = RleDecode.test }
    , { identifier = "14", title = "Duplicate", testFunction = Duplicate.test }
    , { identifier = "15", title = "Repeat elements", testFunction = RepeatElements.test }
    ]


type alias Problem =
    { identifier : String
    , title : String
    , testFunction : Int
    }


main : Html a
main =
    ul [] (problems |> List.map evaluate)


evaluate : Problem -> Html a
evaluate { identifier, title, testFunction } =
    li []
        [ Html.text <|
            identifier
                ++ ". "
                ++ title
                ++ ": "
                ++ (case testFunction of
                        0 ->
                            "Your implementation passed all tests."

                        1 ->
                            "Your implementation failed one test."

                        x ->
                            "Your implementation failed " ++ String.fromInt x ++ " tests."
                   )
        ]
