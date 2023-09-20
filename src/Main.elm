module Main exposing (Model, Msg(..), Problem, main)

import Browser exposing (Document)
import Css
import Html.Styled exposing (Html, button, code, div, h4, input, label, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode
import Random
import Solutions.P1LastElement
import Styles exposing (problemListStyles, problemStyles)
import Utils



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init =
            always
                ( Model []
                    initProblems
                    "[]"
                , Random.generate P1RandomListReady (Random.list 10 (Random.int 1 100))
                )
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
    { p1List : List Int
    , problems : List Problem
    , p1input : String
    }



-- UPDATE


type Msg
    = P1RequestRandomList
    | P1RandomListReady (List Int)
    | P1InputUpdate String
    | P1InputBlur


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        P1RequestRandomList ->
            ( model, Random.generate P1RandomListReady (Random.list 10 (Random.int 1 100)) )

        P1RandomListReady randomList ->
            ( { model
                | p1List = randomList
                , p1input = randomList |> Utils.listToString String.fromInt ", "
              }
            , Cmd.none
            )

        P1InputUpdate input ->
            let
                decodeResult =
                    Json.Decode.decodeString (Json.Decode.list Json.Decode.int) input

                newList =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.p1List
            in
            ( { model
                | p1input = input
                , p1List = newList
              }
            , Cmd.none
            )

        P1InputBlur ->
            ( { model | p1input = model.p1List |> Utils.listToString String.fromInt ", " }, Cmd.none )



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
                , p []
                    [ text "Write a function "
                    , code [ css [ Css.backgroundColor (Css.hex "#f5f7f9"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ] ] [ text "last" ]
                    , text " that returns the last element of a list. An empty list doesn't have a last element, therefore"
                    , code [ css [ Css.backgroundColor (Css.hex "#f5f7f9"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ] ] [ text "last" ]
                    , text " must return a "
                    , code [ css [ Css.backgroundColor (Css.hex "#f5f7f9"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ] ] [ text "Maybe" ]
                    , text "."
                    ]
                , div
                    [ css
                        [ Css.displayFlex
                        , Css.margin4 (Css.px 15) (Css.px 0) (Css.px 15) (Css.px 0)
                        ]
                    ]
                    [ label [ css [ Css.marginRight (Css.px 5) ] ] [ text "Input list: " ]
                    , input
                        [ css [ Css.flex (Css.int 1) ]
                        , onInput P1InputUpdate
                        , onBlur P1InputBlur
                        , value model.p1input
                        ]
                        []
                    , button [ css [ Css.marginLeft (Css.px 5) ], onClick P1RequestRandomList ] [ text "Random" ]
                    ]
                , label [] [ text <| "Last element is: " ]
                , code [ css [ Css.backgroundColor (Css.hex "#f5f7f9"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ] ]
                    [ text <| (Solutions.P1LastElement.last model.p1List |> Utils.maybeToString String.fromInt) ]
                , button [ css [ Css.display Css.block, Css.marginTop (Css.px 15) ] ] [ text "Show code" ]
                ]

        23 ->
            li [ css problemStyles ]
                [ h4 [] [ text ("23. " ++ "Random list elements") ]
                , button [ onClick P1RequestRandomList ] [ text "Generate list" ]
                , span [] (model.p1List |> List.map (\randomNumber -> text (String.fromInt randomNumber ++ " ")))
                ]

        _ ->
            li
                [ css problemStyles ]
                [ h4 [] [ text <| String.fromInt number ++ ". " ++ title ]
                , p [] [ text "Problem requirement here." ]
                ]
