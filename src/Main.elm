module Main exposing (Model, Msg(..), main)

import Array exposing (Array)
import Browser
import Html.Styled as Html exposing (Html, code, div, fromUnstyled, h3, header, input, label, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, id, maxlength, value)
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
import Solutions.P15RepeatElements
import Solutions.P1LastElement
import Solutions.P2Penultimate
import Solutions.P3ElementAt
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P8NoDupes
import Solutions.P9Pack
import SpecialProblems.P10RunLengths as P10RunLengths
import SpecialProblems.P12RleDecode as P12RleDecode
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
        , secondaryInputStyles
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
                |> Array.set 15 [ 1, 2, 3 ]

        secondaryInputs =
            Array.repeat 100 5
                |> Array.set 15 3

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
      , secondaryInputs = secondaryInputs
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , secondaryInputStrings = Array.map String.fromInt secondaryInputs
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      , p7model = P7FlattenNestedList.initModel (problemInfo 7)
      , p10model = P10RunLengths.initModel (problemInfo 10)
      , p12model = P12RleDecode.initModel (problemInfo 12)
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { searchKeyWord : String
    , inputLists : Array (List Int)
    , secondaryInputs : Array Int
    , inputStrings : Array String
    , secondaryInputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p7model : P7FlattenNestedList.Model
    , p10model : P10RunLengths.Model
    , p12model : P12RleDecode.Model
    }



-- UPDATE


type Msg
    = DecodeBasicInput Int String
    | UpdateBasicInput Int
    | GenerateBasicRandomList Int
    | BasicRandomListReady Int (List Int)
    | DecodeBasicSecondaryInput Int String
    | UpdateBasicSecondaryInput Int
    | GenerateRandomSecondaryInput Int
    | RandomSecondaryInputReady Int Int
    | ShowCodeToggle Int
    | SearchProblem String
    | P7Msg P7FlattenNestedList.Msg
    | P10Msg P10RunLengths.Msg
    | P12Msg P12RleDecode.Msg


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

        GenerateRandomSecondaryInput problemNumber ->
            let
                cmd =
                    case problemNumber of
                        3 ->
                            Random.generate (RandomSecondaryInputReady problemNumber)
                                (Random.int 1
                                    (model.inputLists
                                        |> Array.get problemNumber
                                        |> Maybe.withDefault []
                                        |> List.length
                                    )
                                )

                        15 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 3)

                        _ ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 10)
            in
            ( model, cmd )

        RandomSecondaryInputReady problemNumber randomInt ->
            ( { model
                | secondaryInputs = model.secondaryInputs |> Array.set problemNumber randomInt
                , secondaryInputStrings =
                    model.secondaryInputStrings |> Array.set problemNumber (randomInt |> String.fromInt)
              }
            , Cmd.none
            )

        DecodeBasicSecondaryInput problemNumber input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0
            in
            ( { model
                | secondaryInputStrings = model.secondaryInputStrings |> Array.set problemNumber input
                , secondaryInputs = model.secondaryInputs |> Array.set problemNumber newInt
              }
            , Cmd.none
            )

        UpdateBasicSecondaryInput problemNumber ->
            ( { model
                | secondaryInputStrings =
                    model.secondaryInputStrings
                        |> Array.set problemNumber
                            (model.secondaryInputs
                                |> Array.get problemNumber
                                |> Maybe.map String.fromInt
                                |> Maybe.withDefault "0"
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
        , problemInteractiveArea model problem.number
        , viewCodeButton model.showCode problem.number
        , Utils.displayIf (model.showCode |> Array.get problem.number |> Maybe.withDefault False) <|
            (model.solutionsCode
                |> Array.get problem.number
                |> Maybe.withDefault "Error when indexing code for this solution."
                |> HtmlUtils.viewCode
            )
        ]


problemInteractiveArea : Model -> Int -> Html Msg
problemInteractiveArea model problemNumber =
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

        secondaryInput labelText =
            div [ css inputRowStyles ]
                [ label [ css inputLabelStyles ] [ text labelText ]
                , input
                    [ css secondaryInputStyles
                    , onInput (DecodeBasicSecondaryInput problemNumber)
                    , onBlur (UpdateBasicSecondaryInput problemNumber)
                    , value (model.secondaryInputStrings |> Array.get problemNumber |> Maybe.withDefault "0")
                    , maxlength 3
                    ]
                    []
                , niceButton SvgItems.dice "Random" (GenerateRandomSecondaryInput problemNumber)
                ]

        displayResult basicListFunc toString =
            code [ css codeStyles ]
                [ text <|
                    (basicListFunc (model.inputLists |> Array.get problemNumber |> Maybe.withDefault [])
                        |> toString
                    )
                ]

        displayResultWithSecondaryInput basicListFunc toString =
            code [ css codeStyles ]
                [ text <|
                    (basicListFunc (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                        (model.inputLists |> Array.get problemNumber |> Maybe.withDefault [])
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
                [ basicListInput
                , secondaryInput "Index: "
                , label [] [ text "Element: " ]
                , displayResultWithSecondaryInput Solutions.P3ElementAt.elementAt (Utils.maybeToString String.fromInt)
                ]

            4 ->
                [ basicListInput
                , label [] [ text "Count: " ]
                , displayResult Solutions.P4CountElements.countElements String.fromInt
                ]

            5 ->
                [ basicListInput
                , label [] [ text "Reversed list: " ]
                , displayResult Solutions.P5Reverse.myReverse (Utils.listToString String.fromInt ", ")
                ]

            6 ->
                [ basicListInput
                , label [] [ text "Is palindrome: " ]
                , displayResult Solutions.P6IsPalindrome.isPalindrome Utils.boolToString
                ]

            7 ->
                P7FlattenNestedList.specialProblemInteractiveArea model.p7model
                    |> List.map (Html.map P7Msg)

            8 ->
                [ basicListInput
                , label [] [ text "Duplicates removed: " ]
                , displayResult Solutions.P8NoDupes.noDupes (Utils.listToString String.fromInt ", ")
                ]

            9 ->
                [ basicListInput
                , label [] [ text "Duplicates packed: " ]
                , displayResult Solutions.P9Pack.pack Utils.listOfListsToString
                ]

            10 ->
                P10RunLengths.specialProblemInteractiveArea model.p10model |> List.map (Html.map P10Msg)

            11 ->
                [ basicListInput
                , label [] [ text "Encoded: " ]
                , displayResult Solutions.P11RleEncode.rleEncode (Utils.listToString Utils.rleCodeToString ", ")
                ]

            12 ->
                P12RleDecode.specialProblemInteractiveArea model.p12model |> List.map (Html.map P12Msg)

            14 ->
                [ basicListInput
                , label [] [ text "Duplicated: " ]
                , displayResult Solutions.P14Duplicate.duplicate (Utils.listToString String.fromInt ", ")
                ]

            15 ->
                [ basicListInput
                , secondaryInput "Repeat times: "
                , label [] [ text "Repeated elements: " ]
                , displayResultWithSecondaryInput Solutions.P15RepeatElements.repeatElements
                    (Utils.listToString String.fromInt ", ")
                ]

            _ ->
                [ basicListInput
                , label [] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
                ]
