module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css
import Html.Styled exposing (Html, button, code, div, fromUnstyled, h4, input, label, li, p, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode
import Random
import Solutions.P1LastElement
import Styles exposing (codeStyles, problemListStyles, problemStyles, syntaxHighlightRequiredCssNode, syntaxHighlightThemeCssNode)
import SyntaxHighlight
import Utils



-- MAIN


main : Program (Array String) Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }


init : Array String -> ( Model, Cmd Msg )
init flags =
    ( { p1List = []
      , problems = initProblems
      , p1Input = "[]"
      , p1ShowCode = False
      , solutionsCode = flags
      }
    , Random.generate P1RandomListReady (Random.list 10 (Random.int 1 100))
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
    { p1List : List Int
    , problems : List Problem
    , p1Input : String
    , p1ShowCode : Bool
    , solutionsCode : Array String
    }



-- UPDATE


type Msg
    = P1RequestRandomList
    | P1RandomListReady (List Int)
    | P1InputUpdate String
    | P1InputBlur
    | P1ShowCodeToggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        P1RequestRandomList ->
            ( model, Random.generate P1RandomListReady (Random.list 10 (Random.int 1 100)) )

        P1RandomListReady randomList ->
            ( { model
                | p1List = randomList
                , p1Input = randomList |> Utils.listToString String.fromInt ", "
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
                | p1Input = input
                , p1List = newList
              }
            , Cmd.none
            )

        P1InputBlur ->
            ( { model | p1Input = model.p1List |> Utils.listToString String.fromInt ", " }, Cmd.none )

        P1ShowCodeToggle ->
            ( { model | p1ShowCode = model.p1ShowCode |> not }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    ul [ css problemListStyles ] <| (model.problems |> List.map (viewProblem model))


viewProblem : Model -> Problem -> Html Msg
viewProblem model problem =
    case problem.number of
        1 ->
            li
                [ css problemStyles ]
                [ h4 [] [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
                , p []
                    [ text "Write a function "
                    , code [ css codeStyles ] [ text "last" ]
                    , text " that returns the last element of a list. An empty list doesn't have a last element, therefore "
                    , code [ css codeStyles ] [ text "last" ]
                    , text " must return a "
                    , code [ css codeStyles ] [ text "Maybe" ]
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
                        , value model.p1Input
                        ]
                        []
                    , button [ css [ Css.marginLeft (Css.px 5) ], onClick P1RequestRandomList ] [ text "Random" ]
                    ]
                , label [] [ text <| "Last element is: " ]
                , code [ css codeStyles ]
                    [ text <| (Solutions.P1LastElement.last model.p1List |> Utils.maybeToString String.fromInt) ]
                , button [ css [ Css.display Css.block, Css.marginTop (Css.px 15) ], onClick P1ShowCodeToggle ]
                    [ if model.p1ShowCode then
                        text "Hide code"

                      else
                        text "Show code (spoiler)"
                    ]
                , Utils.displayIf model.p1ShowCode <|
                    viewCode model.solutionsCode problem.number
                ]

        _ ->
            li
                [ css problemStyles ]
                [ h4 [] [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
                , p []
                    [ text "Problem requirement here" ]
                , div
                    [ css
                        [ Css.displayFlex
                        , Css.margin4 (Css.px 15) (Css.px 0) (Css.px 15) (Css.px 0)
                        ]
                    ]
                    [ label [ css [ Css.marginRight (Css.px 5) ] ] [ text "Input list: " ]
                    , input
                        [ css [ Css.flex (Css.int 1) ] ]
                        []
                    , button [ css [ Css.marginLeft (Css.px 5) ] ] [ text "Random" ]
                    ]
                , label [] [ text <| "Result is: " ]
                , code [ css codeStyles ]
                    [ text <| "Result goes here" ]
                , button [ css [ Css.display Css.block, Css.marginTop (Css.px 15) ] ]
                    [ text "Hide code (disabled)" ]
                , viewCode model.solutionsCode problem.number
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
