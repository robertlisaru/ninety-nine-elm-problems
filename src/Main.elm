module Main exposing (Model, Msg(..), main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onResize)
import Css exposing (auto, marginLeft, marginTop, px)
import Html.Styled as Html exposing (Html, a, code, div, fromUnstyled, h3, header, input, label, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, href, id, maxlength, target, value)
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
import Solutions.P16DropNth
import Solutions.P17Split
import Solutions.P18Sublist
import Solutions.P19Rotate
import Solutions.P1LastElement
import Solutions.P20DropAt
import Solutions.P21InsertAt
import Solutions.P22Range
import Solutions.P26Combinations
import Solutions.P2Penultimate
import Solutions.P31IsPrime
import Solutions.P32GCD
import Solutions.P33Coprimes
import Solutions.P34Totient
import Solutions.P35PrimeFactors
import Solutions.P36PrimeFactorsM
import Solutions.P37TotientImproved
import Solutions.P39PrimesInRange
import Solutions.P3ElementAt
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P8NoDupes
import Solutions.P9Pack
import SpecialProblems.P10RunLengths as P10RunLengths
import SpecialProblems.P12RleDecode as P12RleDecode
import SpecialProblems.P23RandomSelect as P23RandomSelect
import SpecialProblems.P24Lotto as P24Lotto
import SpecialProblems.P28SortBy as P28SortBy
import SpecialProblems.P38BenchmarkTotient as P38BenchmarkTotient
import SpecialProblems.P7FlattenNestedList as P7FlattenNestedList
import Styles
    exposing
        ( buttonStyles
        , codeStyles
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


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions =
            always (Sub.batch [ onResize UpdateWindowSize ])
        }


type alias Flags =
    { solutions : Array String
    , windowWidth : Int
    , windowHeight : Int
    }


init : Flags -> ( Model, Cmd Msg )
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
                |> Array.set 26 [ 1, 2, 3, 4, 5 ]

        secondaryInputs =
            Array.repeat 100 5
                |> Array.set 15 3
                |> Array.set 16 2
                |> Array.set 18 3
                |> Array.set 22 1
                |> Array.set 26 3
                |> Array.set 32 9
                |> Array.set 33 15
                |> Array.set 34 100
                |> Array.set 35 900
                |> Array.set 36 900
                |> Array.set 37 100
                |> Array.set 39 50

        thirdInputs =
            Array.repeat 100 5
                |> Array.set 18 7
                |> Array.set 21 99
                |> Array.set 22 10
                |> Array.set 32 12
                |> Array.set 33 14
                |> Array.set 34 100
                |> Array.set 37 100
                |> Array.set 39 100

        problemTitle problemNumber =
            problemHeaders
                |> List.filter (\problemHeader -> problemHeader.number == problemNumber)
                |> List.map .title
                |> List.head
                |> Maybe.withDefault "Untitled"

        problemInfo problemNumber =
            { problemNumber = problemNumber
            , problemTitle = problemTitle problemNumber
            }

        ( p23model, p23cmd ) =
            P23RandomSelect.initModel (problemInfo 23)

        ( p24model, p24cmd ) =
            P24Lotto.initModel (problemInfo 24)
    in
    ( { windowSize = { width = flags.windowWidth, height = flags.windowHeight }
      , searchKeyWord = ""
      , inputLists = inputLists
      , secondaryInputs = secondaryInputs
      , thirdInputs = thirdInputs
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , secondaryInputStrings = Array.map String.fromInt secondaryInputs
      , thirdInputStrings = Array.map String.fromInt thirdInputs
      , showCode = Array.repeat 100 False
      , solutionsCode = flags.solutions
      , p7model = P7FlattenNestedList.initModel (problemInfo 7)
      , p10model = P10RunLengths.initModel (problemInfo 10)
      , p12model = P12RleDecode.initModel (problemInfo 12)
      , p23model = p23model
      , p24model = p24model
      , p28model = P28SortBy.initModel (problemInfo 28)
      , p38model = P38BenchmarkTotient.initModel (problemInfo 38)
      }
    , Cmd.batch
        [ p23cmd |> Cmd.map P23Msg
        , p24cmd |> Cmd.map P24Msg
        ]
    )



-- MODEL


type alias Model =
    { windowSize : { width : Int, height : Int }
    , searchKeyWord : String
    , inputLists : Array (List Int)
    , secondaryInputs : Array Int
    , thirdInputs : Array Int
    , inputStrings : Array String
    , secondaryInputStrings : Array String
    , thirdInputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p7model : P7FlattenNestedList.Model
    , p10model : P10RunLengths.Model
    , p12model : P12RleDecode.Model
    , p23model : P23RandomSelect.Model
    , p24model : P24Lotto.Model
    , p28model : P28SortBy.Model
    , p38model : P38BenchmarkTotient.Model
    }



-- UPDATE


type Msg
    = UpdateWindowSize Int Int
    | DecodeBasicInput Int String
    | UpdateBasicInput Int
    | GenerateBasicRandomList Int
    | BasicRandomListReady Int (List Int)
    | DecodeBasicSecondaryInput Int String
    | UpdateBasicSecondaryInput Int
    | GenerateRandomSecondaryInput Int
    | RandomSecondaryInputReady Int Int
    | DecodeBasicThirdInput Int String
    | UpdateBasicThirdInput Int
    | GenerateRandomThirdInput Int
    | RandomThirdInputReady Int Int
    | ShowCodeToggle Int
    | SearchProblem String
    | P7Msg P7FlattenNestedList.Msg
    | P10Msg P10RunLengths.Msg
    | P12Msg P12RleDecode.Msg
    | P23Msg P23RandomSelect.Msg
    | P24Msg P24Lotto.Msg
    | P28Msg P28SortBy.Msg
    | P38Msg P38BenchmarkTotient.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWindowSize width height ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )

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

                        26 ->
                            Random.generate (BasicRandomListReady problemNumber) <| RandomUtils.uniques 3 6 1 10

                        _ ->
                            Random.generate (BasicRandomListReady problemNumber) <| RandomUtils.randomList 10
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

                        26 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 3)

                        31 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (RandomUtils.sometimesPrime 999)

                        32 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        33 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        34 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        35 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        36 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        37 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 0 999)

                        39 ->
                            Random.generate (RandomSecondaryInputReady problemNumber) (Random.int 2 500)

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

        GenerateRandomThirdInput problemNumber ->
            let
                cmd =
                    case problemNumber of
                        21 ->
                            Random.generate (RandomThirdInputReady problemNumber) (Random.int 100 999)

                        32 ->
                            Random.generate (RandomThirdInputReady problemNumber) (Random.int 0 999)

                        33 ->
                            Random.generate (RandomThirdInputReady problemNumber) (Random.int 0 999)

                        39 ->
                            Random.generate (RandomThirdInputReady problemNumber) (Random.int 501 999)

                        _ ->
                            Random.generate (RandomThirdInputReady problemNumber) (Random.int 0 10)
            in
            ( model, cmd )

        RandomThirdInputReady problemNumber randomInt ->
            ( { model
                | thirdInputs = model.thirdInputs |> Array.set problemNumber randomInt
                , thirdInputStrings =
                    model.thirdInputStrings |> Array.set problemNumber (randomInt |> String.fromInt)
              }
            , Cmd.none
            )

        DecodeBasicThirdInput problemNumber input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0
            in
            ( { model
                | thirdInputStrings = model.thirdInputStrings |> Array.set problemNumber input
                , thirdInputs = model.thirdInputs |> Array.set problemNumber newInt
              }
            , Cmd.none
            )

        UpdateBasicThirdInput problemNumber ->
            ( { model
                | thirdInputStrings =
                    model.thirdInputStrings
                        |> Array.set problemNumber
                            (model.thirdInputs
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

        P23Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P23RandomSelect.update problemMsg model.p23model
            in
            ( { model | p23model = newProblemModel }, problemCmd |> Cmd.map P23Msg )

        P24Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P24Lotto.update problemMsg model.p24model
            in
            ( { model | p24model = newProblemModel }, problemCmd |> Cmd.map P24Msg )

        P28Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P28SortBy.update problemMsg model.p28model
            in
            ( { model | p28model = newProblemModel }, problemCmd |> Cmd.map P28Msg )

        P38Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    P38BenchmarkTotient.update problemMsg model.p38model
            in
            ( { model | p38model = newProblemModel }, problemCmd |> Cmd.map P38Msg )



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
            , (Utils.displayIf <| not <| Utils.isMobileDevice <| model.windowSize)
                (sideBarView model.searchKeyWord SearchProblem)
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
        [ h3 [ css problemTitleStyles ]
            [ text <| String.fromInt problem.number ++ ". " ++ problem.title
            , a
                [ href
                    ("https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/p/p"
                        ++ Utils.intToTwoDigitString problem.number
                        ++ ".html"
                    )
                , target "_blank"
                , css
                    (buttonStyles
                        ++ [ marginLeft auto
                           , marginTop (px 3)
                           ]
                    )
                ]
                [ SvgItems.book ]
            ]
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
                , niceButton SvgItems.dice "" (GenerateBasicRandomList problemNumber)
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
                , niceButton SvgItems.dice "" (GenerateRandomSecondaryInput problemNumber)
                ]

        thirdInput labelText =
            div [ css inputRowStyles ]
                [ label [ css inputLabelStyles ] [ text labelText ]
                , input
                    [ css secondaryInputStyles
                    , onInput (DecodeBasicThirdInput problemNumber)
                    , onBlur (UpdateBasicThirdInput problemNumber)
                    , value (model.thirdInputStrings |> Array.get problemNumber |> Maybe.withDefault "0")
                    , maxlength 3
                    ]
                    []
                , niceButton SvgItems.dice "" (GenerateRandomThirdInput problemNumber)
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

        displayResultWithThirdInput basicListFunc toString =
            code [ css codeStyles ]
                [ text <|
                    (basicListFunc (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                        (model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                        (model.inputLists |> Array.get problemNumber |> Maybe.withDefault [])
                        |> toString
                    )
                ]
    in
    div [ css problemInteractiveAreaStyles ] <|
        case problemNumber of
            1 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Last element is: " ]
                , displayResult Solutions.P1LastElement.last (Utils.maybeToString String.fromInt)
                ]

            2 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Penultimate element is: " ]
                , displayResult Solutions.P2Penultimate.penultimate (Utils.maybeToString String.fromInt)
                ]

            3 ->
                [ basicListInput
                , secondaryInput "Index: "
                , label [ css inputLabelStyles ] [ text "Element: " ]
                , displayResultWithSecondaryInput Solutions.P3ElementAt.elementAt (Utils.maybeToString String.fromInt)
                ]

            4 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Count: " ]
                , displayResult Solutions.P4CountElements.countElements String.fromInt
                ]

            5 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Reversed list: " ]
                , displayResult Solutions.P5Reverse.myReverse (Utils.listToString String.fromInt ", ")
                ]

            6 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Is palindrome: " ]
                , displayResult Solutions.P6IsPalindrome.isPalindrome Utils.boolToString
                ]

            7 ->
                P7FlattenNestedList.specialProblemInteractiveArea model.p7model
                    |> List.map (Html.map P7Msg)

            8 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Duplicates removed: " ]
                , displayResult Solutions.P8NoDupes.noDupes (Utils.listToString String.fromInt ", ")
                ]

            9 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Duplicates packed: " ]
                , displayResult Solutions.P9Pack.pack Utils.listOfListsToString
                ]

            10 ->
                P10RunLengths.specialProblemInteractiveArea model.p10model |> List.map (Html.map P10Msg)

            11 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Encoded: " ]
                , displayResult Solutions.P11RleEncode.rleEncode (Utils.listToString Utils.rleCodeToString ", ")
                ]

            12 ->
                P12RleDecode.specialProblemInteractiveArea model.p12model |> List.map (Html.map P12Msg)

            14 ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Duplicated: " ]
                , displayResult Solutions.P14Duplicate.duplicate (Utils.listToString String.fromInt ", ")
                ]

            15 ->
                [ basicListInput
                , secondaryInput "Repeat times: "
                , label [ css inputLabelStyles ] [ text "Repeated elements: " ]
                , displayResultWithSecondaryInput Solutions.P15RepeatElements.repeatElements
                    (Utils.listToString String.fromInt ", ")
                ]

            16 ->
                [ basicListInput
                , secondaryInput "n: "
                , label [ css inputLabelStyles ] [ text "n-th elements removed: " ]
                , displayResultWithSecondaryInput Solutions.P16DropNth.dropNth
                    (Utils.listToString String.fromInt ", ")
                ]

            17 ->
                [ basicListInput
                , secondaryInput "Length of first part: "
                , label [ css inputLabelStyles ] [ text "Split: " ]
                , displayResultWithSecondaryInput Solutions.P17Split.split
                    (Utils.tupleToString
                        (Utils.listToString String.fromInt ", ")
                        (Utils.listToString String.fromInt ", ")
                    )
                ]

            18 ->
                [ basicListInput
                , secondaryInput "Start: "
                , thirdInput "End: "
                , label [ css inputLabelStyles ] [ text "Sublist: " ]
                , displayResultWithThirdInput Solutions.P18Sublist.sublist
                    (Utils.listToString String.fromInt ", ")
                ]

            19 ->
                [ basicListInput
                , secondaryInput "n: "
                , label [ css inputLabelStyles ] [ text "Rotated list: " ]
                , displayResultWithSecondaryInput Solutions.P19Rotate.rotate
                    (Utils.listToString String.fromInt ", ")
                ]

            20 ->
                [ basicListInput
                , secondaryInput "n: "
                , label [ css inputLabelStyles ] [ text "n-th element removed: " ]
                , displayResultWithSecondaryInput Solutions.P20DropAt.dropAt
                    (Utils.listToString String.fromInt ", ")
                ]

            21 ->
                [ basicListInput
                , thirdInput "New element: "
                , secondaryInput "Position: "
                , label [ css inputLabelStyles ] [ text "Added new element: " ]
                , displayResultWithThirdInput Solutions.P21InsertAt.insertAt
                    (Utils.listToString String.fromInt ", ")
                ]

            22 ->
                [ secondaryInput "Start: "
                , thirdInput "End: "
                , label [ css inputLabelStyles ] [ text "Integers in range: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P22Range.range (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            (model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> Utils.listToString String.fromInt ", "
                        )
                    ]
                ]

            23 ->
                P23RandomSelect.specialProblemInteractiveArea model.p23model |> List.map (Html.map P23Msg)

            24 ->
                P24Lotto.specialProblemInteractiveArea model.p24model |> List.map (Html.map P24Msg)

            26 ->
                [ basicListInput
                , secondaryInput "n: "
                , label [ css inputLabelStyles ] [ text "Combinations: " ]
                , displayResultWithSecondaryInput Solutions.P26Combinations.combinations
                    Utils.listOfListsToString
                ]

            28 ->
                P28SortBy.specialProblemInteractiveArea model.p28model |> List.map (Html.map P28Msg)

            31 ->
                [ secondaryInput "n: "
                , label [ css inputLabelStyles ] [ text "Is prime: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P31IsPrime.isPrime
                            (model.secondaryInputs
                                |> Array.get problemNumber
                                |> Maybe.withDefault 0
                            )
                            |> Utils.boolToString
                        )
                    ]
                ]

            32 ->
                [ secondaryInput "a: "
                , thirdInput "b: "
                , label [ css inputLabelStyles ] [ text "GCD: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P32GCD.gcd (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            (model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> String.fromInt
                        )
                    ]
                ]

            33 ->
                [ secondaryInput "a: "
                , thirdInput "b: "
                , label [ css inputLabelStyles ] [ text "Coprimes: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P33Coprimes.coprimes (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            (model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> Utils.boolToString
                        )
                    ]
                ]

            34 ->
                [ secondaryInput "m: "
                , label [ css inputLabelStyles ] [ text "Totient: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P34Totient.totient (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> String.fromInt
                        )
                    ]
                ]

            35 ->
                [ secondaryInput "m: "
                , label [ css inputLabelStyles ] [ text "Prime factors: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P35PrimeFactors.primeFactors (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> Utils.listToString String.fromInt ", "
                        )
                    ]
                ]

            36 ->
                [ secondaryInput "m: "
                , label [ css inputLabelStyles ] [ text "Prime factors: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P36PrimeFactorsM.primeFactorsM (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> Utils.listToString (Utils.tupleToString String.fromInt String.fromInt) ", "
                        )
                    ]
                ]

            37 ->
                [ secondaryInput "m: "
                , label [ css inputLabelStyles ] [ text "Totient: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P37TotientImproved.totient (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> String.fromInt
                        )
                    ]
                ]

            38 ->
                P38BenchmarkTotient.specialProblemInteractiveArea model.p38model |> List.map (Html.map P38Msg)

            39 ->
                [ secondaryInput "Start: "
                , thirdInput "End: "
                , label [ css inputLabelStyles ] [ text "Primes in range: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P39PrimesInRange.primesInRange (model.secondaryInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            (model.thirdInputs |> Array.get problemNumber |> Maybe.withDefault 0)
                            |> Utils.listToString String.fromInt ", "
                        )
                    ]
                ]

            _ ->
                [ basicListInput
                , label [ css inputLabelStyles ] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
                ]
