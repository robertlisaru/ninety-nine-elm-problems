module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css exposing (displayFlex, margin4, px)
import Html.Styled exposing (Html, button, code, div, fromUnstyled, h2, header, input, label, li, p, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode
import Random
import Solutions.P1LastElement
import Styles exposing (codeStyles, headerStyles, problemInteractiveAreaStyles, problemListStyles, problemStyles, problemTitleStyles, syntaxHighlightRequiredCssNode, syntaxHighlightThemeCssNode)
import SyntaxHighlight
import Utils



-- MAIN


main : Program (Array String) Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Array String -> ( Model, Cmd Msg )
init flags =
    ( { inputList = Array.repeat 100 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      , problems = initProblems
      , input = Array.repeat 100 "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      }
    , Cmd.none
    )



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
    { inputList : Array (List Int)
    , problems : List Problem
    , input : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    }



-- UPDATE


type Msg
    = RequestRandomList Int
    | RandomListReady Int (List Int)
    | InputUpdate Int String
    | InputBlur Int
    | ShowCodeToggle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestRandomList problemNumber ->
            ( model, Random.generate (RandomListReady problemNumber) (Random.list 10 (Random.int 1 100)) )

        RandomListReady problemNumber randomList ->
            ( { model
                | inputList = model.inputList |> Array.set problemNumber randomList
                , input = model.input |> Array.set problemNumber (randomList |> Utils.listToString String.fromInt ", ")
              }
            , Cmd.none
            )

        InputUpdate problemNumber input ->
            let
                decodeResult =
                    Json.Decode.decodeString (Json.Decode.list Json.Decode.int) input

                newList =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.inputList |> Array.get problemNumber |> Maybe.withDefault []
            in
            ( { model
                | input = model.input |> Array.set problemNumber input
                , inputList = model.inputList |> Array.set problemNumber newList
              }
            , Cmd.none
            )

        InputBlur problemNumber ->
            ( { model
                | input =
                    model.input
                        |> Array.set problemNumber
                            (model.inputList
                                |> Array.get problemNumber
                                |> Maybe.map (Utils.listToString String.fromInt ", ")
                                |> Maybe.withDefault "[]"
                            )
              }
            , Cmd.none
            )

        ShowCodeToggle problemNumber ->
            let
                flipped =
                    model.showCode |> Array.get problemNumber |> Maybe.map not |> Maybe.withDefault False
            in
            ( { model | showCode = model.showCode |> Array.set problemNumber flipped }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "99 Elm problems"
    , body =
        [ header [ css headerStyles ] []
        , ul [ css problemListStyles ] <| (model.problems |> List.map (viewProblem model))
        ]
            |> List.map toUnstyled
    }


viewProblem : Model -> Problem -> Html Msg
viewProblem model problem =
    li
        [ css problemStyles ]
        [ h2 [ css problemTitleStyles ] [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
        , problemRequirement problem.number
        , problemInteractiveArea model problem.number
        , viewCodeButton model.showCode problem.number
        , Utils.displayIf (model.showCode |> Array.get problem.number |> Maybe.withDefault False) <|
            viewCode model.solutionsCode problem.number
        ]


problemRequirement : Int -> Html Msg
problemRequirement problemNumber =
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

        _ ->
            p [] [ text "Problem requirement here" ]


problemInteractiveArea : Model -> Int -> Html Msg
problemInteractiveArea model problemNumber =
    div [ css problemInteractiveAreaStyles ] <|
        case problemNumber of
            1 ->
                [ div
                    [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0) ] ]
                    [ label [ css [ Css.marginRight (Css.px 5) ] ] [ text "Input list: " ]
                    , input
                        [ css [ Css.flex (Css.int 1) ]
                        , onInput (InputUpdate problemNumber)
                        , onBlur (InputBlur problemNumber)
                        , value (model.input |> Array.get problemNumber |> Maybe.withDefault "[]")
                        ]
                        []
                    , button
                        [ css [ Css.marginLeft (Css.px 5) ]
                        , onClick (RequestRandomList problemNumber)
                        ]
                        [ text "Random" ]
                    ]
                , label [] [ text <| "Last element is: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P1LastElement.last
                            (model.inputList |> Array.get problemNumber |> Maybe.withDefault [])
                            |> Utils.maybeToString String.fromInt
                        )
                    ]
                ]

            _ ->
                [ div
                    [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0) ] ]
                    [ label [ css [ Css.marginRight (Css.px 5) ] ] [ text "Input list: " ]
                    , input
                        [ css [ Css.flex (Css.int 1) ]
                        , onInput (InputUpdate problemNumber)
                        , onBlur (InputBlur problemNumber)
                        , value (model.input |> Array.get problemNumber |> Maybe.withDefault "[]")
                        ]
                        []
                    , button
                        [ css [ Css.marginLeft (Css.px 5) ]
                        , onClick (RequestRandomList problemNumber)
                        ]
                        [ text "Random" ]
                    ]
                , label [] [ text <| "Result is: " ]
                , code [ css codeStyles ]
                    [ text <| "Result goes here" ]
                ]


viewCodeButton : Array Bool -> Int -> Html Msg
viewCodeButton showCode problemNumber =
    button [ css [ Css.display Css.block, Css.marginTop (Css.px 15) ], onClick (ShowCodeToggle problemNumber) ]
        [ if showCode |> Array.get problemNumber |> Maybe.withDefault False then
            text "Hide code"

          else
            text "Show code (spoiler)"
        ]


viewCode : Array String -> Int -> Html Msg
viewCode solutionsCode problemNumber =
    div [ css [ Css.marginTop (Css.px 15) ] ]
        [ syntaxHighlightRequiredCssNode
        , syntaxHighlightThemeCssNode --overriden by SyntaxHighlight.useTheme
        , SyntaxHighlight.useTheme SyntaxHighlight.gitHub |> fromUnstyled
        , SyntaxHighlight.elm
            (solutionsCode
                |> Array.get problemNumber
                |> Maybe.withDefault "Error when indexing code for this solution."
            )
            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
            |> Result.map fromUnstyled
            |> Result.withDefault (code [] [ text "Syntax highlight error." ])
        ]
