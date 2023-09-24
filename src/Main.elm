module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css exposing (block, display, displayFlex, flex, margin4, marginLeft, marginRight, marginTop, px, width)
import Html.Styled exposing (Html, button, code, div, fromUnstyled, h2, header, input, label, li, p, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode
import Random
import Solutions.P1LastElement
import Solutions.P2Penultimate
import Solutions.P3ElementAt
import Solutions.P4CountElements
import Styles
    exposing
        ( codeStyles
        , headerStyles
        , problemInteractiveAreaStyles
        , problemListStyles
        , problemStyles
        , problemTitleStyles
        , syntaxHighlightRequiredCssNode
        , syntaxHighlightThemeCssNode
        )
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
      , p3Index = 7
      , p3IndexString = "7"
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
    , p3Index : Int
    , p3IndexString : String
    }



-- UPDATE


type Msg
    = RequestRandomList Int
    | RandomListReady Int (List Int)
    | InputUpdate Int String
    | InputBlur Int
    | ShowCodeToggle Int
    | P3RequestRandomIndex
    | P3RandomIndexReady Int
    | P3IndexInput String
    | P3IndexBlur


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

        P3RequestRandomIndex ->
            ( model
            , Random.generate P3RandomIndexReady
                (Random.int 1 (model.inputList |> Array.get 3 |> Maybe.map List.length |> Maybe.withDefault 10))
            )

        P3RandomIndexReady randomIndex ->
            ( { model | p3Index = randomIndex, p3IndexString = randomIndex |> String.fromInt }, Cmd.none )

        P3IndexInput input ->
            let
                decodeResult =
                    Json.Decode.decodeString Json.Decode.int input

                newIndex =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.p3Index
            in
            ( { model
                | p3IndexString = input
                , p3Index = newIndex
              }
            , Cmd.none
            )

        P3IndexBlur ->
            ( { model | p3IndexString = model.p3Index |> String.fromInt }
            , Cmd.none
            )



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

        _ ->
            p [] [ text "Problem requirement here" ]


problemInteractiveArea : Model -> Int -> Html Msg
problemInteractiveArea model problemNumber =
    let
        basicListInput =
            div
                [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0) ] ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input list: " ]
                , input
                    [ css [ flex (Css.int 1) ]
                    , onInput (InputUpdate problemNumber)
                    , onBlur (InputBlur problemNumber)
                    , value (model.input |> Array.get problemNumber |> Maybe.withDefault "[]")
                    ]
                    []
                , button
                    [ css [ marginLeft (px 5) ], onClick (RequestRandomList problemNumber) ]
                    [ text "Random" ]
                ]

        displayResult basicListFunc toString =
            code [ css codeStyles ]
                [ text <|
                    (basicListFunc (model.inputList |> Array.get problemNumber |> Maybe.withDefault [])
                        |> toString
                    )
                ]
    in
    div [ css problemInteractiveAreaStyles ] <|
        case problemNumber of
            1 ->
                [ basicListInput
                , label [] [ text "Last element is: " ]
                , displayResult Solutions.P1LastElement.last (Utils.maybeToString String.fromInt)
                ]

            2 ->
                [ basicListInput
                , label [] [ text "Penultimate element is: " ]
                , displayResult Solutions.P2Penultimate.penultimate (Utils.maybeToString String.fromInt)
                ]

            3 ->
                let
                    p3IndexInput =
                        div
                            [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0) ] ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Index: " ]
                            , input
                                [ css [ width (Css.em 3) ]
                                , onInput P3IndexInput
                                , onBlur P3IndexBlur
                                , value model.p3IndexString
                                , maxlength 3
                                ]
                                []
                            , button
                                [ css [ marginLeft (px 5) ], onClick P3RequestRandomIndex ]
                                [ text "Random" ]
                            ]
                in
                [ basicListInput
                , p3IndexInput
                , label [] [ text "Indexed element is: " ]
                , displayResult (Solutions.P3ElementAt.elementAt model.p3Index) (Utils.maybeToString String.fromInt)
                ]

            4 ->
                [ basicListInput
                , label [] [ text "Count: " ]
                , displayResult Solutions.P4CountElements.countElements String.fromInt
                ]

            _ ->
                [ basicListInput
                , label [] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
                ]


viewCodeButton : Array Bool -> Int -> Html Msg
viewCodeButton showCode problemNumber =
    button [ css [ display block, marginTop (px 15) ], onClick (ShowCodeToggle problemNumber) ]
        [ if showCode |> Array.get problemNumber |> Maybe.withDefault False then
            text "Hide code"

          else
            text "Show code (spoiler)"
        ]


viewCode : Array String -> Int -> Html Msg
viewCode solutionsCode problemNumber =
    div [ css [ marginTop (px 15) ] ]
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
