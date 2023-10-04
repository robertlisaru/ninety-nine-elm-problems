module Main exposing (Model, Msg(..), ProblemHeader, main)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, a, code, div, fromUnstyled, h1, h2, h3, header, input, label, li, nav, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, href, id, placeholder, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
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
            , sideBarView model.searchKeyWord
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


sideBarView : String -> Html Msg
sideBarView searchKeyWord =
    let
        filteredProblems =
            problemHeaders
                |> List.filter
                    (\problem ->
                        (String.fromInt problem.number ++ ". " ++ problem.title)
                            |> String.toLower
                            |> String.contains (searchKeyWord |> String.toLower)
                    )

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
                [ css listInputAreaStyles ]
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

            11 ->
                [ basicListInput
                , label [] [ text "Encoded: " ]
                , displayResult Solutions.P11RleEncode.rleEncode (Utils.listToString Utils.rleCodeToString ", ")
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
