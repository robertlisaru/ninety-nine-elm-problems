module Main exposing (..)

import Browser
import Html exposing (Html, button, li, span, text, ul)
import Html.Events exposing (onClick)
import Problems.CountElements
import Problems.DropAt
import Problems.DropNth
import Problems.Duplicate
import Problems.ElementAt
import Problems.FlattenNestedList
import Problems.InsertAt
import Problems.IsPalindrome
import Problems.LastElement
import Problems.NoDupes
import Problems.Pack
import Problems.Penultimate
import Problems.Range
import Problems.RepeatElements
import Problems.Reverse
import Problems.RleDecode
import Problems.RleEncode
import Problems.Rotate
import Problems.RunLengths
import Problems.Split
import Problems.Sublist
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( Model [], Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { randomList : List Int
    }


type alias Problem =
    { identifier : String
    , title : String
    , testFunction : Int
    }


problems : List Problem
problems =
    [ { identifier = "1", title = "Last element", testFunction = Problems.LastElement.test }
    , { identifier = "2", title = "Penultimate", testFunction = Problems.Penultimate.test }
    , { identifier = "3", title = "Element at", testFunction = Problems.ElementAt.test }
    , { identifier = "4", title = "Count elements", testFunction = Problems.CountElements.test }
    , { identifier = "5", title = "Reverse", testFunction = Problems.Reverse.test }
    , { identifier = "6", title = "Is palindrome", testFunction = Problems.IsPalindrome.test }
    , { identifier = "7", title = "Flatten nested list", testFunction = Problems.FlattenNestedList.test }
    , { identifier = "8", title = "No dupes", testFunction = Problems.NoDupes.test }
    , { identifier = "9", title = "Pack", testFunction = Problems.Pack.test }
    , { identifier = "10", title = "Run lengths", testFunction = Problems.RunLengths.test }
    , { identifier = "11", title = "Run lengths encode", testFunction = Problems.RleEncode.test }
    , { identifier = "12", title = "Run lengths decode", testFunction = Problems.RleDecode.test }
    , { identifier = "14", title = "Duplicate", testFunction = Problems.Duplicate.test }
    , { identifier = "15", title = "Repeat elements", testFunction = Problems.RepeatElements.test }
    , { identifier = "16", title = "Drop nth", testFunction = Problems.DropNth.test }
    , { identifier = "17", title = "Split", testFunction = Problems.Split.test }
    , { identifier = "18", title = "Sublist", testFunction = Problems.Sublist.test }
    , { identifier = "19", title = "Rotate", testFunction = Problems.Rotate.test }
    , { identifier = "20", title = "Drop at", testFunction = Problems.DropAt.test }
    , { identifier = "21", title = "Insert at", testFunction = Problems.InsertAt.test }
    , { identifier = "22", title = "Range", testFunction = Problems.Range.test }
    , { identifier = "23", title = "Random select", testFunction = 0 }
    ]



-- UPDATE


type Msg
    = RequestRandomList
    | RandomListArrived (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestRandomList ->
            ( model, Random.generate RandomListArrived (Random.list 10 (Random.int 1 100)) )

        RandomListArrived randomList ->
            ( { model | randomList = randomList }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    ul [] (problems |> List.map (evaluate model))


evaluate : Model -> Problem -> Html Msg
evaluate model { identifier, title, testFunction } =
    case identifier of
        "23" ->
            problem23 model

        _ ->
            li []
                [ text <|
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


problem23 : Model -> Html Msg
problem23 model =
    li []
        [ text ("23. " ++ "Random list elements")
        , button [ onClick RequestRandomList ] [ text "Generate list" ]
        , span [] (model.randomList |> List.map (\number -> text (String.fromInt number ++ " ")))
        ]
