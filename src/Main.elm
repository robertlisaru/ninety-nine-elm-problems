module Main exposing (Model, Msg(..), Problem, main)

import Browser exposing (Document)
import Css exposing (alignItems, auto, borderRadius, borderStyle, borderWidth, boxShadow4, center, column, displayFlex, flexDirection, listStyleType, marginBottom, marginLeft, marginRight, marginTop, maxWidth, minHeight, minWidth, none, padding, pct, px, rgba, solid, width)
import Html.Styled exposing (Html, button, li, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always ( Model [], Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
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


view : Model -> Document Msg
view model =
    { title = "Ninety-nine Elm solutions"
    , body =
        [ ul
            [ css
                [ listStyleType none
                , padding (px 0)
                , marginTop (px 25)
                , marginLeft auto
                , marginRight auto
                , width (pct 100)
                , displayFlex
                , flexDirection column
                , alignItems center
                ]
            ]
            (problems |> List.map (viewProblem model))
        ]
            |> List.map toUnstyled
    }


viewProblem : Model -> Problem -> Html Msg
viewProblem model { identifier, title } =
    case identifier of
        "23" ->
            problem23 model

        _ ->
            li
                [ css
                    [ borderWidth (px 2)
                    , borderStyle solid
                    , borderRadius (px 15)
                    , boxShadow4 (px 5) (px 5) (px 5) (rgba 0 0 0 0.5)
                    , marginBottom (px 25)
                    , maxWidth (px 775)
                    , minWidth (px 300)
                    , width (pct 80)
                    , minHeight (px 300)
                    , padding (px 25)
                    ]
                ]
                [ text <| identifier ++ ". " ++ title ]


problem23 : Model -> Html Msg
problem23 model =
    li []
        [ text ("23. " ++ "Random list elements")
        , button [ onClick RequestRandomList ] [ text "Generate list" ]
        , span [] (model.randomList |> List.map (\number -> text (String.fromInt number ++ " ")))
        ]
