module Main exposing (Model, Msg(..), ProblemHeader, main)

import Array exposing (Array)
import Browser
import Css exposing (..)
import DecoderUtils
import Html.Styled as Html exposing (Html, a, code, div, fromUnstyled, h1, h2, h3, header, input, label, li, nav, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, href, id, placeholder, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import ProblemText
import Problems.P15RepeatElements
import Problems.P1LastElement
import Problems.P3ElementAt
import Random
import RandomUtils
import Solutions.P10RunLengths
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P12RleDecode
import Solutions.P14Duplicate
import Solutions.P2Penultimate
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P7FlattenNestedList exposing (NestedList(..))
import Solutions.P8NoDupes
import Solutions.P9Pack
import Styles
    exposing
        ( codeStyles
        , genericStylesNode
        , headerStyles
        , leftContentStyles
        , linkStyles
        , listInputAreaStyles
        , listInputStyles
        , navStyles
        , pageContainerStyles
        , problemInteractiveAreaStyles
        , problemListStyles
        , problemStyles
        , problemTitleStyles
        , searchBarStyles
        , sideBarItemListStyles
        , sideBarStyles
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

        p7nestedList =
            SubList
                [ Elem 1
                , SubList [ SubList [ Elem 2, SubList [ Elem 3, Elem 4 ] ], Elem 5 ]
                , Elem 6
                , SubList [ Elem 7, Elem 8, Elem 9 ]
                ]

        p10listOfLists =
            [ [ 1, 1 ], [ 2, 2, 2 ] ]

        p12rleCodes : List (RleCode Int)
        p12rleCodes =
            [ Run 2 1, Run 3 2 ]

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
    in
    ( { searchKeyWord = ""
      , inputLists = inputLists
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      , p7nestedList = p7nestedList
      , p7inputString = p7nestedList |> Utils.nestedListToString
      , p10listOfLists = p10listOfLists
      , p10inputString = p10listOfLists |> Utils.listOfListsToString
      , p12rleCodes = p12rleCodes
      , p12inputString = p12rleCodes |> Utils.listToString Utils.rleCodeToString ", "
      , p1model = Problems.P1LastElement.initModel 1 (problemTitle 1) (solutionCode 1)
      , p3model = Problems.P3ElementAt.initModel 3 (problemTitle 3) (solutionCode 3)
      , p15model = Problems.P15RepeatElements.initModel 15 (problemTitle 15) (solutionCode 15)
      }
    , Cmd.none
    )



-- MODEL


type alias ProblemHeader =
    { number : Int
    , title : String
    }


problemHeaders : List ProblemHeader
problemHeaders =
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
    { searchKeyWord : String
    , inputLists : Array (List Int)
    , inputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p7nestedList : NestedList Int
    , p7inputString : String
    , p10listOfLists : List (List Int)
    , p10inputString : String
    , p12rleCodes : List (RleCode Int)
    , p12inputString : String
    , p1model : Problems.P1LastElement.Model
    , p3model : Problems.P3ElementAt.Model
    , p15model : Problems.P15RepeatElements.Model
    }



-- UPDATE


type Msg
    = GenerateRandomList Int
    | RandomListReady Int (List Int)
    | InputUpdate Int String
    | InputBlur Int
    | ShowCodeToggle Int
    | P7InputUpdate String
    | P7InputBlur
    | P7GenerateRandomNestedList
    | P7RandomNestedListReady (NestedList Int)
    | P10InputUpdate String
    | P10InputBlur
    | P10GenerateRandomListOfLists
    | P10RandomListOfListsReady (List (List Int))
    | P12InputUpdate String
    | P12InputBlur
    | P12GenerateRandomRleCodes
    | P12RleCodesReady (List (RleCode Int))
    | SearchProblem String
    | P1Msg Problems.P1LastElement.Msg
    | P3Msg Problems.P3ElementAt.Msg
    | P15Msg Problems.P15RepeatElements.Msg


generateRandomListCmd : Int -> Cmd Msg
generateRandomListCmd problemNumber =
    case problemNumber of
        6 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.sometimesPalindrome

        8 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        9 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        11 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        _ ->
            Random.generate (RandomListReady problemNumber) (RandomUtils.randomList 10)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomList problemNumber ->
            ( model, generateRandomListCmd problemNumber )

        RandomListReady problemNumber randomList ->
            ( { model
                | inputLists = model.inputLists |> Array.set problemNumber randomList
                , inputStrings =
                    model.inputStrings
                        |> Array.set problemNumber
                            (randomList |> Utils.listToString String.fromInt ", ")
              }
            , Cmd.none
            )

        InputUpdate problemNumber input ->
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

        InputBlur problemNumber ->
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

        P7InputUpdate input ->
            let
                decodeResult =
                    Decode.decodeString DecoderUtils.nestedListDecoder input

                newNestedList =
                    case decodeResult of
                        Result.Ok nestedList ->
                            nestedList

                        Err _ ->
                            model.p7nestedList
            in
            ( { model
                | p7inputString = input
                , p7nestedList = newNestedList
              }
            , Cmd.none
            )

        P7InputBlur ->
            ( { model | p7inputString = model.p7nestedList |> Utils.nestedListToString }
            , Cmd.none
            )

        P7GenerateRandomNestedList ->
            ( model
            , Random.generate P7RandomNestedListReady (RandomUtils.nestedListGenerator 1.0)
            )

        P7RandomNestedListReady nestedList ->
            ( { model
                | p7nestedList = nestedList
                , p7inputString = nestedList |> Utils.nestedListToString
              }
            , Cmd.none
            )

        P10InputUpdate input ->
            let
                decodeResult =
                    Decode.decodeString DecoderUtils.decodeDuplicates input

                newNestedList =
                    case decodeResult of
                        Result.Ok nestedList ->
                            nestedList

                        Err _ ->
                            model.p10listOfLists
            in
            ( { model
                | p10inputString = input
                , p10listOfLists = newNestedList
              }
            , Cmd.none
            )

        P10InputBlur ->
            ( { model | p10inputString = model.p10listOfLists |> Utils.listOfListsToString }
            , Cmd.none
            )

        P10GenerateRandomListOfLists ->
            ( model
            , Random.generate P10RandomListOfListsReady (RandomUtils.duplicateSequences |> Random.map Solutions.P9Pack.pack)
            )

        P10RandomListOfListsReady listOfLists ->
            ( { model
                | p10listOfLists = listOfLists
                , p10inputString = listOfLists |> Utils.listOfListsToString
              }
            , Cmd.none
            )

        SearchProblem keyWord ->
            ( { model | searchKeyWord = keyWord }, Cmd.none )

        P12InputUpdate input ->
            let
                decodeResult =
                    (input |> String.replace "[" "[\"" |> String.replace "]" "\"]" |> String.replace "," "\", \"")
                        |> Decode.decodeString (Decode.list DecoderUtils.rleDecoder)

                newRleCodes =
                    case decodeResult of
                        Result.Ok rleCodes ->
                            rleCodes

                        Err _ ->
                            model.p12rleCodes
            in
            ( { model
                | p12inputString = input
                , p12rleCodes = newRleCodes
              }
            , Cmd.none
            )

        P12InputBlur ->
            ( { model | p12inputString = model.p12rleCodes |> Utils.listToString Utils.rleCodeToString ", " }
            , Cmd.none
            )

        P12GenerateRandomRleCodes ->
            ( model
            , Random.generate P12RleCodesReady (RandomUtils.duplicateSequences |> Random.map Solutions.P11RleEncode.rleEncode)
            )

        P12RleCodesReady rleCodes ->
            ( { model
                | p12rleCodes = rleCodes
                , p12inputString = rleCodes |> Utils.listToString Utils.rleCodeToString ", "
              }
            , Cmd.none
            )

        P1Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    Problems.P1LastElement.update problemMsg model.p1model
            in
            ( { model | p1model = newProblemModel }, problemCmd |> Cmd.map P1Msg )

        P3Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    Problems.P3ElementAt.update problemMsg model.p3model
            in
            ( { model | p3model = newProblemModel }, problemCmd |> Cmd.map P3Msg )

        P15Msg problemMsg ->
            let
                ( newProblemModel, problemCmd ) =
                    Problems.P15RepeatElements.update problemMsg model.p15model
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
            [ div [ css leftContentStyles ]
                [ appIntroView
                , ul [ css problemListStyles ] <|
                    (problemHeaders
                        |> List.map
                            (\problemHeader ->
                                case problemHeader.number of
                                    1 ->
                                        Problems.P1LastElement.view model.p1model |> Html.map P1Msg

                                    3 ->
                                        Problems.P3ElementAt.view model.p3model |> Html.map P3Msg

                                    15 ->
                                        Problems.P15RepeatElements.view model.p15model |> Html.map P15Msg

                                    _ ->
                                        viewBasicProblem model problemHeader
                            )
                    )
                ]
            , sideBarView
                (problemHeaders
                    |> List.filter
                        (\problem ->
                            (String.fromInt problem.number ++ ". " ++ problem.title)
                                |> String.toLower
                                |> String.contains (model.searchKeyWord |> String.toLower)
                        )
                )
                model.searchKeyWord
            ]
        ]
            |> List.map toUnstyled
    }


navView : Html Msg
navView =
    let
        navItem url label =
            a
                [ css [ textDecoration none, color (hex "#ffffff"), hover [ textDecoration underline ] ]
                , href url
                ]
                [ text label ]
    in
    nav [ css navStyles ]
        [ logoView
        , h1 [ css [ fontSize (px 24), fontWeight normal, color (hex "#ffffff") ] ]
            [ navItem "https://github.com/robertlisaru" "robertlisaru"
            , span [ css [ margin2 (px 0) (px 10) ] ] [ text "/" ]
            , navItem "https://github.com/robertlisaru/ninety-nine-elm-problems" "ninety-nine-elm-problems"
            ]
        ]


logoView : Html Msg
logoView =
    a
        [ css
            [ textDecoration none
            , marginRight (px 32)
            , displayFlex
            , alignItems center
            ]
        ]
        [ SvgItems.elmLogo
        , div [ css [ paddingLeft (px 8), color (hex "#ffffff") ] ]
            [ div [ css [ lineHeight (px 24), fontSize (px 30) ] ] [ text "elm" ]
            , div [ css [ fontSize (px 12) ] ] [ text "99 problems" ]
            ]
        ]


appIntroView : Html Msg
appIntroView =
    div []
        [ h1 [ css [ fontSize (em 3), marginBottom (px 0), fontWeight normal ] ] [ text "99 Elm problems" ]
        , h2 [ css [ fontSize (px 16), marginTop (px 0), marginBottom (px 50), lineHeight (em 1.5), fontWeight normal ] ]
            [ text "Sharpen your functional programming skills." ]
        ]


sideBarView : List ProblemHeader -> String -> Html Msg
sideBarView filteredProblems searchKeyWord =
    let
        linkItem url label =
            li [] [ a [ href url, css linkStyles ] [ text label ] ]
    in
    div [ css sideBarStyles ]
        [ ul [ css sideBarItemListStyles ]
            [ linkItem "" "README"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "About"
            , linkItem "https://github.com/robertlisaru/ninety-nine-elm-problems" "Source"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Credits" ]
        , ul [ css sideBarItemListStyles ]
            [ linkItem "https://github.com/evancz" "Elm was created by Evan Czaplicki"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "The 99 problems are adapted to Elm in a gitbook by johncrane"
            , linkItem "https://package.elm-lang.org/" "This page layout is inspired by the official Elm Packages website"
            , linkItem "https://elm-lang.org/" "Visit the official Elm Website"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Problems" ]
        , input [ placeholder "Search", Html.Styled.Attributes.type_ "search", css searchBarStyles, value searchKeyWord, onInput SearchProblem ] []
        , ul [ css sideBarItemListStyles ]
            (filteredProblems
                |> List.map
                    (\problem ->
                        li []
                            [ a
                                [ href ("#" ++ (problem.number |> String.fromInt))
                                , css linkStyles
                                ]
                                [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
                            ]
                    )
            )
        , div [ css [ position sticky, top (px 20), marginTop (px 20) ] ]
            [ a
                [ css
                    (linkStyles
                        ++ [ displayFlex
                           , alignItems center
                           ]
                    )
                , href "#"
                ]
                [ SvgItems.top
                , div [ css [ marginRight (em 0.6) ] ] []
                , text "Back to top"
                ]
            ]
        ]


viewBasicProblem : Model -> ProblemHeader -> Html Msg
viewBasicProblem model problem =
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
                [ css listInputAreaStyles ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input list: " ]
                , input
                    [ css listInputStyles
                    , onInput (InputUpdate problemNumber)
                    , onBlur (InputBlur problemNumber)
                    , value (model.inputStrings |> Array.get problemNumber |> Maybe.withDefault "[]")
                    ]
                    []
                , niceButton SvgItems.dice "Random" (GenerateRandomList problemNumber)
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
        case problemNumber of
            2 ->
                [ basicListInput
                , label [] [ text "Penultimate element is: " ]
                , displayResult Solutions.P2Penultimate.penultimate (Utils.maybeToString String.fromInt)
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
                let
                    nestedListInput =
                        div
                            [ css listInputAreaStyles ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Input nested list: " ]
                            , input
                                [ css listInputStyles
                                , onInput P7InputUpdate
                                , onBlur P7InputBlur
                                , value model.p7inputString
                                ]
                                []
                            , niceButton SvgItems.dice "Random" P7GenerateRandomNestedList
                            ]
                in
                [ nestedListInput
                , label [] [ text "Flattened list: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P7FlattenNestedList.flatten model.p7nestedList
                            |> Utils.listToString String.fromInt ", "
                        )
                    ]
                ]

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
                let
                    listOfListsInput =
                        div
                            [ css listInputAreaStyles ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Input duplicates: " ]
                            , input
                                [ css listInputStyles
                                , onInput P10InputUpdate
                                , onBlur P10InputBlur
                                , value model.p10inputString
                                ]
                                []
                            , niceButton SvgItems.dice "Random" P10GenerateRandomListOfLists
                            ]
                in
                [ listOfListsInput
                , label [] [ text "Run lengths: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P10RunLengths.runLengths model.p10listOfLists
                            |> Utils.listToString Utils.tupleToString ", "
                        )
                    ]
                ]

            11 ->
                [ basicListInput
                , label [] [ text "Encoded: " ]
                , displayResult Solutions.P11RleEncode.rleEncode (Utils.listToString Utils.rleCodeToString ", ")
                ]

            12 ->
                let
                    rleCodesInput =
                        div
                            [ css listInputAreaStyles ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Input codes: " ]
                            , input
                                [ css listInputStyles
                                , onInput P12InputUpdate
                                , onBlur P12InputBlur
                                , value model.p12inputString
                                ]
                                []
                            , niceButton SvgItems.dice "Random" P12GenerateRandomRleCodes
                            ]
                in
                [ rleCodesInput
                , label [] [ text "Decoded: " ]
                , code [ css codeStyles ]
                    [ text <|
                        (Solutions.P12RleDecode.rleDecode model.p12rleCodes
                            |> Utils.listToString String.fromInt ", "
                        )
                    ]
                ]

            14 ->
                [ basicListInput
                , label [] [ text "Duplicated: " ]
                , displayResult Solutions.P14Duplicate.duplicate (Utils.listToString String.fromInt ", ")
                ]

            _ ->
                [ basicListInput
                , label [] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
                ]
