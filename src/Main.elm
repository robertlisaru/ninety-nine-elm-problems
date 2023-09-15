module Main exposing (Model, Msg(..), Problem, main)

import Browser
import Html exposing (Html, button, li, span, text, ul)
import Html.Events exposing (onClick)
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
    }


problems : List Problem
problems =
    [ { identifier = "1", title = "Last element" }
    , { identifier = "2", title = "Penultimate" }
    , { identifier = "3", title = "Element at" }
    , { identifier = "4", title = "Count elements" }
    , { identifier = "5", title = "Reverse" }
    , { identifier = "6", title = "Is palindrome" }
    , { identifier = "7", title = "Flatten nested list" }
    , { identifier = "8", title = "No dupes" }
    , { identifier = "9", title = "Pack" }
    , { identifier = "10", title = "Run lengths" }
    , { identifier = "11", title = "Run lengths encode" }
    , { identifier = "12", title = "Run lengths decode" }
    , { identifier = "14", title = "Duplicate" }
    , { identifier = "15", title = "Repeat elements" }
    , { identifier = "16", title = "Drop nth" }
    , { identifier = "17", title = "Split" }
    , { identifier = "18", title = "Sublist" }
    , { identifier = "19", title = "Rotate" }
    , { identifier = "20", title = "Drop at" }
    , { identifier = "21", title = "Insert at" }
    , { identifier = "22", title = "Range" }
    , { identifier = "23", title = "Random select" }
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
    ul [] (problems |> List.map (viewProblem model))


viewProblem : Model -> Problem -> Html Msg
viewProblem model { identifier, title } =
    case identifier of
        "23" ->
            problem23 model

        _ ->
            li [] [ text <| identifier ++ ". " ++ title ]


problem23 : Model -> Html Msg
problem23 model =
    li []
        [ text ("23. " ++ "Random list elements")
        , button [ onClick RequestRandomList ] [ text "Generate list" ]
        , span [] (model.randomList |> List.map (\number -> text (String.fromInt number ++ " ")))
        ]
