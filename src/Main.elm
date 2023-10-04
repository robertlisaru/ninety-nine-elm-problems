module Main exposing (Model, Msg(..), main)

import Array exposing (Array)
import Browser
import Html.Styled as Html exposing (Html, code, div, fromUnstyled, h3, header, input, label, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, id, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import PageElements exposing (appIntroView, navView, sideBarView)
import ProblemHeaders exposing (ProblemHeader, problemHeaders)
import ProblemText
import Random
import RandomUtils
import Solutions.P11RleEncode
import Solutions.P14Duplicate
import Solutions.P1LastElement
import Solutions.P2Penultimate
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P8NoDupes
import Solutions.P9Pack
import SpecialProblems.P10RunLengths as P10RunLengths
import SpecialProblems.P12RleDecode as P12RleDecode
import SpecialProblems.P15RepeatElements as P15RepeatElements
import SpecialProblems.P3ElementAt as P3ElementAt
import SpecialProblems.P7FlattenNestedList as P7FlattenNestedList
import Styles
    exposing
        ( codeStyles
        , genericStylesNode
        , headerStyles
        , inputLabelStyles
        , inputRowStyles
        , leftContentStyles
        , listInputStyles
        , pageContainerStyles
        , problemInteractiveAreaStyles
        , problemListStyles
        , problemStyles
        , problemTitleStyles
        , syntaxHighlightRequiredCssNode
        , syntaxHighlightThemeCssNode
        )
import SvgItems
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
    let
        inputLists =
            Array.repeat 100 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                |> Array.set 6 [ 1, 2, 3, 2, 1 ]
                |> Array.set 8 [ 1, 1, 2, 2, 2 ]
                |> Array.set 9 [ 1, 1, 2, 2, 2 ]
                |> Array.set 11 [ 1, 1, 2, 2, 2 ]
                |> Array.set 14 [ 1, 2, 3, 4, 5 ]

        solutionCode problemNumber =
            flags
                |> Array.get problemNumber
                |> Maybe.withDefault
                    ("-- No code found for problem " ++ String.fromInt problemNumber)

        problemTitle problemNumber =
            problemHeaders
                |> List.filter (\problemHeader -> problemHeader.number == problemNumber)
                |> List.map .title
                |> List.head
                |> Maybe.withDefault "Untitled"

        problemInfo problemNumber =
            { problemNumber = problemNumber
            , problemTitle = problemTitle problemNumber
            , solutionCode = solutionCode problemNumber
            }
    in
    ( { searchKeyWord = ""
      , inputLists = inputLists
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      , p3model = P3ElementAt.initModel (problemInfo 3)
      , p7model = P7FlattenNestedList.initModel (problemInfo 7)
      , p10model = P10RunLengths.initModel (problemInfo 10)
      , p12model = P12RleDecode.initModel (problemInfo 12)
      , p15model = P15RepeatElements.initModel (problemInfo 15)
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { searchKeyWord : String
    , inputLists : Array (List Int)
    , inputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p3model : P3ElementAt.Model
    , p7model : P7FlattenNestedList.Model
    , p10model : P10RunLengths.Model
    , p12model : P12RleDecode.Model
    , p15model : P15RepeatElements.Model
    }



-- UPDATE


type Msg
    = GenerateBasicRandomList Int
    | BasicRandomListReady Int (List Int)
    | DecodeBasicInput Int String
    | UpdateBasicInput Int
    | ShowCodeToggle Int
    | SearchProblem String
    | P3Msg P3ElementAt.Msg
    | P7Msg P7FlattenNestedList.Msg
    | P10Msg P10RunLengths.Msg
    | P12Msg P12RleDecode.Msg
    | P15Msg P15RepeatElements.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBasicRandomList problemNumber ->
            let
                cmd =
                    case problemNumber of
                        6 ->
                            Random.generate (BasicRandomListReady problemNumber) RandomUtils.sometimesPalindrome

                        8 ->
                            Random.generate (BasicRandomListReady problemNumber) RandomUtils.duplicateSequences

                        9 ->
                            Random.generate (BasicRandomListReady problemNumber) RandomUtils.duplicateSequences

                        11 ->
                            Random.generate (BasicRandomListReady problemNumber) RandomUtils.duplicateSequences

                        _ ->
                            Random.generate (BasicRandomListReady problemNumber) (RandomUtils.randomList 10)
            in
            ( model, cmd )

        BasicRandomListReady problemNumber randomList ->
            ( { model
                | inputLists = model.inputLists |> Array.set problemNumber randomList
                , inputStrings =
                    model.inputStrings
                        |> Array.set problemNumber
                            (randomList |> Utils.listToString String.fromInt ", ")
              }
            , Cmd.none
            )

        DecodeBasicInput problemNumber input ->
            let
                decodeResult =
                    Decode.decodeString (Decode.list Decode.int) input

                newList =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.inputLists |> Array.get problemNumber |> Maybe.withDefault []
            in
            ( { model
                | inputStrings = model.inputStrings |> Array.set problemNumber input
                , inputLists = model.inputLists |> Array.set problemNumber newList
              }
            , Cmd.none
            )

        UpdateBasicInput problemNumber ->
            ( { model
                | inputStrings =
                    model.inputStrings
                        |> Array.set problemNumber
                            (model.inputLists
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

        SearchProblem keyWord ->
            ( { model | searchKeyWord = keyWord }, Cmd.none )

        P3Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P3ElementAt.update problemMsg model.p3model
            in
            ( { model | p3model = newProblemModel }, problemCmd |> Cmd.map P3Msg )

        P7Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P7FlattenNestedList.update problemMsg model.p7model
            in
            ( { model | p7model = newProblemModel }, problemCmd |> Cmd.map P7Msg )

        P10Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P10RunLengths.update problemMsg model.p10model
            in
            ( { model | p10model = newProblemModel }, problemCmd |> Cmd.map P10Msg )

        P12Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P12RleDecode.update problemMsg model.p12model
            in
            ( { model | p12model = newProblemModel }, problemCmd |> Cmd.map P12Msg )

        P15Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P15RepeatElements.update problemMsg model.p15model
            in
            ( { model | p15model = newProblemModel }, problemCmd |> Cmd.map P15Msg )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "99 Elm problems"
    , body =
        [ genericStylesNode
        , syntaxHighlightRequiredCssNode
        , syntaxHighlightThemeCssNode --overriden by SyntaxHighlight.useTheme
        , SyntaxHighlight.useTheme SyntaxHighlight.gitHub |> fromUnstyled
        , header [ css headerStyles ] [ navView ]
        , div [ css pageContainerStyles ]
            [ div [ css leftContentStyles ] [ appIntroView, viewProblems model ]
            , sideBarView model.searchKeyWord SearchProblem
            ]
        ]
            |> List.map toUnstyled
    }


viewProblems : Model -> Html Msg
viewProblems model =
    ul [ css problemListStyles ] <| (problemHeaders |> List.map (viewProblem model))


viewProblem : Model -> ProblemHeader -> Html Msg
viewProblem model problem =
    let
        viewCodeButton showCode problemNumber =
            niceButton SvgItems.elmColoredLogo
                (if showCode |> Array.get problemNumber |> Maybe.withDefault False then
                    "Hide code"

                 else
                    "Show code (spoiler)"
                )
                (ShowCodeToggle problemNumber)
    in
    li
        [ css problemStyles, id (problem.number |> String.fromInt) ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
        , ProblemText.requirement problem.number
        , case problem.number of
            3 ->
                P3ElementAt.specialProblemInteractiveArea model.p3model |> Html.map P3Msg

            7 ->
                P7FlattenNestedList.specialProblemInteractiveArea model.p7model |> Html.map P7Msg

            10 ->
                P10RunLengths.specialProblemInteractiveArea model.p10model |> Html.map P10Msg

            12 ->
                P12RleDecode.specialProblemInteractiveArea model.p12model |> Html.map P12Msg

            15 ->
                P15RepeatElements.specialProblemInteractiveArea model.p15model |> Html.map P15Msg

            _ ->
                basicProblemInteractiveArea model problem.number
        , viewCodeButton model.showCode problem.number
        , Utils.displayIf (model.showCode |> Array.get problem.number |> Maybe.withDefault False) <|
            (model.solutionsCode
                |> Array.get problem.number
                |> Maybe.withDefault "Error when indexing code for this solution."
                |> HtmlUtils.viewCode
            )
        ]


basicProblemInteractiveArea : Model -> Int -> Html Msg
basicProblemInteractiveArea model problemNumber =
    let
        basicListInput =
            div
                [ css inputRowStyles ]
                [ label [ css inputLabelStyles ] [ text "Input list: " ]
                , input
                    [ css listInputStyles
                    , onInput (DecodeBasicInput problemNumber)
                    , onBlur (UpdateBasicInput problemNumber)
                    , value (model.inputStrings |> Array.get problemNumber |> Maybe.withDefault "[]")
                    ]
                    []
                , niceButton SvgItems.dice "Random" (GenerateBasicRandomList problemNumber)
                ]

        displayResult basicListFunc toString =
            code [ css codeStyles ]
                [ text <|
                    (basicListFunc (model.inputLists |> Array.get problemNumber |> Maybe.withDefault [])
                        |> toString
                    )
                ]
    in
    div [ css problemInteractiveAreaStyles ] <|
        basicListInput
            :: (case problemNumber of
                    1 ->
                        [ label [] [ text "Last element is: " ]
                        , displayResult Solutions.P1LastElement.last (Utils.maybeToString String.fromInt)
                        ]

                    2 ->
                        [ label [] [ text "Penultimate element is: " ]
                        , displayResult Solutions.P2Penultimate.penultimate (Utils.maybeToString String.fromInt)
                        ]

                    4 ->
                        [ label [] [ text "Count: " ]
                        , displayResult Solutions.P4CountElements.countElements String.fromInt
                        ]

                    5 ->
                        [ label [] [ text "Reversed list: " ]
                        , displayResult Solutions.P5Reverse.myReverse (Utils.listToString String.fromInt ", ")
                        ]

                    6 ->
                        [ label [] [ text "Is palindrome: " ]
                        , displayResult Solutions.P6IsPalindrome.isPalindrome Utils.boolToString
                        ]

                    8 ->
                        [ label [] [ text "Duplicates removed: " ]
                        , displayResult Solutions.P8NoDupes.noDupes (Utils.listToString String.fromInt ", ")
                        ]

                    9 ->
                        [ label [] [ text "Duplicates packed: " ]
                        , displayResult Solutions.P9Pack.pack Utils.listOfListsToString
                        ]

                    11 ->
                        [ label [] [ text "Encoded: " ]
                        , displayResult Solutions.P11RleEncode.rleEncode (Utils.listToString Utils.rleCodeToString ", ")
                        ]

                    14 ->
                        [ label [] [ text "Duplicated: " ]
                        , displayResult Solutions.P14Duplicate.duplicate (Utils.listToString String.fromInt ", ")
                        ]

                    _ ->
                        [ label [] [ text "Result is: " ]
                        , code [ css codeStyles ] [ text "Result goes here" ]
                        ]
               )
