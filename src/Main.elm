module Main exposing (main)

import CountElements
import ElementAt
import Html exposing (Html, li, ol)
import LastElement
import Penultimate


problems : List Problem
problems =
    [ { identifier = "1", title = "Last element", testFunction = LastElement.test }
    , { identifier = "2", title = "Penultimate", testFunction = Penultimate.test }
    , { identifier = "3", title = "Element at", testFunction = ElementAt.test }
    , { identifier = "4", title = "Count elements", testFunction = CountElements.test }
    ]


type alias Problem =
    { identifier : String
    , title : String
    , testFunction : Int
    }


main : Html a
main =
    ol [] (problems |> List.map evaluate)


evaluate : Problem -> Html a
evaluate { title, testFunction } =
    li []
        [ Html.text <|
            title
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
