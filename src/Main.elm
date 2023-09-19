module Main exposing (Model, Msg(..), Problem, main)

import Browser exposing (Document)
import Css
import Html.Styled exposing (Html, button, div, h4, input, label, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Random
import Styles exposing (problemListStyles, problemStyles)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always ( Model [] initProblems, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Problem =
    { number : Int
    , title : String
    }


initProblems : List Problem
initProblems =
    [ { number = 1, title = "Last element" }
    , { number = 2, title = "Penultimate" }
    , { number = 3, title = "Element at" }
    , { number = 4, title = "Count elements" }
    , { number = 5, title = "Reverse" }
    , { number = 6, title = "Is palindrome" }
    , { number = 7, title = "Flatten nested list" }
    , { number = 8, title = "No dupes" }
    , { number = 9, title = "Pack" }
    , { number = 10, title = "Run lengths" }
    , { number = 11, title = "Run lengths encode" }
    , { number = 12, title = "Run lengths decode" }
    , { number = 14, title = "Duplicate" }
    , { number = 15, title = "Repeat elements" }
    , { number = 16, title = "Drop nth" }
    , { number = 17, title = "Split" }
    , { number = 18, title = "Sublist" }
    , { number = 19, title = "Rotate" }
    , { number = 20, title = "Drop at" }
    , { number = 21, title = "Insert at" }
    , { number = 22, title = "Range" }
    , { number = 23, title = "Random select" }
    ]


type alias Model =
    { randomList : List Int
    , problems : List Problem
    }



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
        [ ul [ css problemListStyles ] <| (model.problems |> List.map (viewProblem model)) ]
            |> List.map toUnstyled
    }


viewProblem : Model -> Problem -> Html Msg
viewProblem model { number, title } =
    case number of
        1 ->
            li
                [ css problemStyles ]
                [ h4 [] [ text <| String.fromInt number ++ ". " ++ title ]
                , p [] [ text "Write a function last that returns the last element of a list. An empty list doesn't have a last element, therefore last must return a Maybe. " ]
                , div
                    [ css
                        [ Css.displayFlex
                        , Css.margin4 (Css.px 15) (Css.px 0) (Css.px 15) (Css.px 0)
                        ]
                    ]
                    [ label [ css [ Css.marginRight (Css.px 5) ] ] [ text "Input list: " ]
                    , input [ css [ Css.flex (Css.int 1) ] ] []
                    , button [ css [ Css.marginLeft (Css.px 5) ] ] [ text "Random" ]
                    ]
                , button [] [ text "Test" ]
                , button [ css [ Css.display Css.block, Css.marginTop (Css.px 15) ] ] [ text "Show code" ]
                ]

        23 ->
            li [ css problemStyles ]
                [ h4 [] [ text ("23. " ++ "Random list elements") ]
                , button [ onClick RequestRandomList ] [ text "Generate list" ]
                , span [] (model.randomList |> List.map (\randomNumber -> text (String.fromInt randomNumber ++ " ")))
                ]

        _ ->
            li
                [ css problemStyles ]
                [ h4 [] [ text <| String.fromInt number ++ ". " ++ title ]
                , p [] [ text "Problem requirement here." ]
                ]
