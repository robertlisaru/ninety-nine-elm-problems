module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css exposing (..)
import DecoderUtils
import Html.Styled
    exposing
        ( Html
        , a
        , button
        , code
        , div
        , fromUnstyled
        , h1
        , h2
        , h3
        , header
        , input
        , label
        , li
        , nav
        , span
        , text
        , toUnstyled
        , ul
        )
import Html.Styled.Attributes exposing (css, href, id, maxlength, placeholder, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import ProblemText
import Random
import RandomUtils
import Solutions.P10RunLengths
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P12RleDecode
import Solutions.P14Duplicate
import Solutions.P15RepeatElements
import Solutions.P1LastElement
import Solutions.P2Penultimate
import Solutions.P3ElementAt
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P7FlattenNestedList exposing (NestedList(..))
import Solutions.P8NoDupes
import Solutions.P9Pack
import Styles
    exposing
        ( buttonStyles
        , codeStyles
        , genericStylesNode
        , headerStyles
        , leftContentStyles
        , linkStyles
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
                |> Array.set 15 [ 1, 2, 3 ]

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
    in
    ( { filteredProblems = problems
      , inputLists = inputLists
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      , p3Index = 5
      , p3IndexString = "5"
      , p7nestedList = p7nestedList
      , p7inputString = p7nestedList |> Utils.nestedListToString
      , p10listOfLists = p10listOfLists
      , p10inputString = p10listOfLists |> Utils.listOfListsToString
      , p12rleCodes = p12rleCodes
      , p12inputString = p12rleCodes |> Utils.listToString Utils.rleCodeToString ", "
      , p15repeatTimes = 3
      , p15repeatTimesString = "3"
      }
    , Cmd.none
    )



-- MODEL


type alias Problem =
    { number : Int
    , title : String
    }


problems : List Problem
problems =
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
    { filteredProblems : List Problem
    , inputLists : Array (List Int)
    , inputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p3Index : Int
    , p3IndexString : String
    , p7nestedList : Solutions.P7FlattenNestedList.NestedList Int
    , p7inputString : String
    , p10listOfLists : List (List Int)
    , p10inputString : String
    , p12rleCodes : List (RleCode Int)
    , p12inputString : String
    , p15repeatTimesString : String
    , p15repeatTimes : Int
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
    | P7InputUpdate String
    | P7InputBlur
    | P7RequestRandomNestedList
    | P7RandomNestedListReady (NestedList Int)
    | P10InputUpdate String
    | P10InputBlur
    | P10RequestRandomListOfLists
    | P10RandomListOfListsReady (List (List Int))
    | P12InputUpdate String
    | P12InputBlur
    | P12RequestRandomRleCodes
    | P12RleCodesReady (List (RleCode Int))
    | SearchProblem String
    | P15RequestRandomRepeatTimes
    | P15RandomRandomRepeatTimesReady Int
    | P15InputRepeatTimes String
    | P15RepeatTimesBlur


requestRandomListCmd : Int -> Cmd Msg
requestRandomListCmd problemNumber =
    case problemNumber of
        6 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.sometimesPalindrome

        8 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        9 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        11 ->
            Random.generate (RandomListReady problemNumber) RandomUtils.duplicateSequences

        15 ->
            Random.generate (RandomListReady problemNumber) (RandomUtils.randomList 5)

        _ ->
            Random.generate (RandomListReady problemNumber) (RandomUtils.randomList 10)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestRandomList problemNumber ->
            ( model, requestRandomListCmd problemNumber )

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

        P3RequestRandomIndex ->
            ( model
            , Random.generate P3RandomIndexReady
                (Random.int 1 (model.inputLists |> Array.get 3 |> Maybe.map List.length |> Maybe.withDefault 10))
            )

        P3RandomIndexReady randomIndex ->
            ( { model | p3Index = randomIndex, p3IndexString = randomIndex |> String.fromInt }, Cmd.none )

        P3IndexInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

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

        P7RequestRandomNestedList ->
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

        P10RequestRandomListOfLists ->
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
            ( { model
                | filteredProblems =
                    problems
                        |> List.filter
                            (.title >> String.toLower >> String.contains (keyWord |> String.toLower))
              }
            , Cmd.none
            )

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

        P12RequestRandomRleCodes ->
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

        P15RequestRandomRepeatTimes ->
            ( model, Random.generate P15RandomRandomRepeatTimesReady (Random.int 0 3) )

        P15RandomRandomRepeatTimesReady randomRepeatTimes ->
            ( { model
                | p15repeatTimes = randomRepeatTimes
                , p15repeatTimesString = randomRepeatTimes |> String.fromInt
              }
            , Cmd.none
            )

        P15InputRepeatTimes input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newRepeatTimes =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.p3Index
            in
            ( { model
                | p15repeatTimesString = input
                , p15repeatTimes = newRepeatTimes
              }
            , Cmd.none
            )

        P15RepeatTimesBlur ->
            ( { model | p15repeatTimesString = model.p15repeatTimes |> String.fromInt }, Cmd.none )



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
                , ul [ css problemListStyles ] <| (problems |> List.map (viewProblem model))
                ]
            , sideBarView model.filteredProblems
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


sideBarView : List Problem -> Html Msg
sideBarView filteredProblems =
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
        , input [ placeholder "Search", css searchBarStyles, onInput SearchProblem ] []
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


niceButton : Html msg -> String -> msg -> Html msg
niceButton icon label onClickMsg =
    button
        [ css buttonStyles, onClick onClickMsg ]
        [ icon
        , if label /= "" then
            span
                [ css [ marginLeft (em 0.2) ] ]
                [ text label ]

          else
            text ""
        ]


viewProblem : Model -> Problem -> Html Msg
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
                |> ProblemText.viewCode
            )
        ]


problemInteractiveArea : Model -> Int -> Html Msg
problemInteractiveArea model problemNumber =
    let
        listInputAreaStyles =
            [ displayFlex
            , marginBottom (px 15)
            , height (px 32)
            , alignItems center
            ]

        listInputStyles =
            [ flex (int 1), marginRight (px 8) ]

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
                , niceButton SvgItems.dice "Random" (RequestRandomList problemNumber)
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
                            [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0), alignItems center ] ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Index: " ]
                            , input
                                [ css [ width (em 3), marginRight (px 8) ]
                                , onInput P3IndexInput
                                , onBlur P3IndexBlur
                                , value model.p3IndexString
                                , maxlength 3
                                ]
                                []
                            , niceButton SvgItems.dice "Random" P3RequestRandomIndex
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
                            , niceButton SvgItems.dice "Random" P7RequestRandomNestedList
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
                            , niceButton SvgItems.dice "Random" P10RequestRandomListOfLists
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
                            , niceButton SvgItems.dice "Random" P12RequestRandomRleCodes
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

            15 ->
                let
                    repeatTimesInput =
                        div
                            [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0), alignItems center ] ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Index: " ]
                            , input
                                [ css [ width (em 3), marginRight (px 8) ]
                                , onInput P15InputRepeatTimes
                                , onBlur P15RepeatTimesBlur
                                , value model.p15repeatTimesString
                                , maxlength 3
                                ]
                                []
                            , niceButton SvgItems.dice "Random" P15RequestRandomRepeatTimes
                            ]
                in
                [ basicListInput
                , repeatTimesInput
                , label [] [ text "Repeat times: " ]
                , displayResult (Solutions.P15RepeatElements.repeatElements model.p15repeatTimes) (Utils.listToString String.fromInt ", ")
                ]

            _ ->
                [ basicListInput
                , label [] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
                ]
