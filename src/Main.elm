module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css exposing (..)
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
        , p
        , span
        , text
        , toUnstyled
        , ul
        )
import Html.Styled.Attributes exposing (css, href, maxlength, placeholder, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Random
import Solutions.P1LastElement
import Solutions.P2Penultimate
import Solutions.P3ElementAt
import Solutions.P4CountElements
import Solutions.P5Reverse
import Solutions.P6IsPalindrome
import Solutions.P7FlattenNestedList exposing (NestedList(..))
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
                |> Array.set 4 [ 1, 2, 3, 4, 5 ]
                |> Array.set 6 [ 1, 2, 3, 4, 5, 4, 3, 2, 1 ]

        p7nestedList =
            SubList
                [ Elem 1
                , SubList [ SubList [ Elem 2, SubList [ Elem 3, Elem 4 ] ], Elem 5 ]
                , Elem 6
                , SubList [ Elem 7, Elem 8, Elem 9 ]
                ]
    in
    ( { inputLists = inputLists
      , problems = initProblems
      , inputStrings = Array.map (Utils.listToString String.fromInt ", ") inputLists
      , showCode = Array.repeat 100 False
      , solutionsCode = flags
      , p3Index = 7
      , p3IndexString = "7"
      , p7nestedList = p7nestedList
      , p7inputString = p7nestedList |> Utils.nestedListToString
      }
    , requestRandomListCmd 3
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
    { inputLists : Array (List Int)
    , problems : List Problem
    , inputStrings : Array String
    , showCode : Array Bool
    , solutionsCode : Array String
    , p3Index : Int
    , p3IndexString : String
    , p7nestedList : Solutions.P7FlattenNestedList.NestedList Int
    , p7inputString : String
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


requestRandomListCmd : Int -> Cmd Msg
requestRandomListCmd problemNumber =
    Random.generate (RandomListReady problemNumber)
        (Random.int 0 10
            |> Random.andThen (\n -> Random.list n (Random.int 1 100))
        )


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
                    Decode.decodeString nestedListDecoder input

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
            , Random.generate P7RandomNestedListReady (randomNestedListGenerator 1.0)
            )

        P7RandomNestedListReady nestedList ->
            ( { model
                | p7nestedList = nestedList
                , p7inputString = nestedList |> Utils.nestedListToString
              }
            , Cmd.none
            )


randomNestedListGenerator : Float -> Random.Generator (NestedList Int)
randomNestedListGenerator initialSubListProbability =
    let
        clampedProbability =
            if initialSubListProbability < 0 then
                0.0

            else if initialSubListProbability > 1.0 then
                1.0

            else
                initialSubListProbability

        maxSubListLength =
            3

        sometimesTrue =
            Random.weighted ( clampedProbability, True ) [ ( 1 - clampedProbability, False ) ]

        randomLength =
            Random.int 1 maxSubListLength

        randomSubList =
            randomLength
                |> Random.andThen randomSubListOfLength
                |> Random.map SubList

        randomSubListOfLength length =
            Random.list length (Random.lazy (\_ -> randomNestedListGenerator (clampedProbability / 2)))

        randomElem =
            Random.int 1 100 |> Random.map Elem
    in
    sometimesTrue
        |> Random.andThen
            (\isTrue ->
                if isTrue then
                    randomSubList

                else
                    randomElem
            )


nestedListDecoder : Decoder (NestedList Int)
nestedListDecoder =
    Decode.oneOf
        [ Decode.int |> Decode.map Elem
        , Decode.list (Decode.lazy (\_ -> nestedListDecoder)) |> Decode.map SubList
        ]



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
                , ul [ css problemListStyles ] <| (model.problems |> List.map (viewProblem model))
                ]
            , sideBarView
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


sideBarView : Html Msg
sideBarView =
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
        , input [ placeholder "Search unimplemented", css searchBarStyles ] []
        , ul [ css sideBarItemListStyles ]
            [ li [] [ text "1. Last element" ]
            , li [] [ text "2. Penultimate" ]
            , li [] [ text "3. Element at" ]
            , li [] [ text "4. Count elements" ]
            , li [] [ text "..." ]
            ]
        ]


viewProblem : Model -> Problem -> Html Msg
viewProblem model problem =
    li
        [ css problemStyles ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
        , problemRequirement problem.number
        , problemInteractiveArea model problem.number
        , viewCodeButton model.showCode problem.number
        , Utils.displayIf (model.showCode |> Array.get problem.number |> Maybe.withDefault False) <|
            (model.solutionsCode
                |> Array.get problem.number
                |> Maybe.withDefault "Error when indexing code for this solution."
                |> viewCode
            )
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

        5 ->
            p []
                [ text "Elm provides the function "
                , code [ css codeStyles ] [ text "List.reverse" ]
                , text " to reverse the order of elements in a list. See if you can implement it."
                ]

        6 ->
            p []
                [ text "Determine if a list is a palindrome, that is, the list is identical when read forward or backward." ]

        7 ->
            p []
                [ text "Flatten a nested lists into a single list. Because Lists in Elm are homogeneous we need to define what a nested list is."
                , viewCode "type NestedList a = Elem a | List [NestedList a]"
                ]

        _ ->
            p [] [ text "Problem requirement here" ]


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

            _ ->
                [ basicListInput
                , label [] [ text "Result is: " ]
                , code [ css codeStyles ] [ text "Result goes here" ]
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


viewCodeButton : Array Bool -> Int -> Html Msg
viewCodeButton showCode problemNumber =
    niceButton SvgItems.elmColoredLogo
        (if showCode |> Array.get problemNumber |> Maybe.withDefault False then
            "Hide code"

         else
            "Show code (spoiler)"
        )
        (ShowCodeToggle problemNumber)


viewCode : String -> Html Msg
viewCode solutionCode =
    div [ css [ marginTop (px 15) ] ]
        [ SyntaxHighlight.elm solutionCode
            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
            |> Result.map fromUnstyled
            |> Result.withDefault (code [] [ text "Syntax highlight error." ])
        ]
