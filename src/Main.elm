module Main exposing (Model, Msg(..), Problem, main)

import Array exposing (Array)
import Browser
import Css exposing (alignItems, auto, block, center, color, display, displayFlex, em, flex, fontSize, fontWeight, height, hex, hover, lineHeight, listStyleType, margin, margin2, margin4, marginBottom, marginLeft, marginRight, marginTop, maxWidth, none, normal, padding, paddingLeft, pct, px, textDecoration, underline, width)
import Html.Styled exposing (Html, a, button, code, div, fromUnstyled, h1, h2, h3, header, input, label, li, nav, p, span, text, toUnstyled, ul)
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
        ( codeStyles
        , headerStyles
        , leftContentStyles
        , pageContainerStyles
        , problemInteractiveAreaStyles
        , problemListStyles
        , problemStyles
        , problemTitleStyles
        , searchBarStyles
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
    Random.generate (RandomListReady problemNumber) (Random.int 0 10 |> Random.andThen (\n -> Random.list n (Random.int 1 100)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestRandomList problemNumber ->
            ( model, requestRandomListCmd problemNumber )

        RandomListReady problemNumber randomList ->
            ( { model
                | inputLists = model.inputLists |> Array.set problemNumber randomList
                , inputStrings = model.inputStrings |> Array.set problemNumber (randomList |> Utils.listToString String.fromInt ", ")
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
        clampedSubListProbability =
            if initialSubListProbability < 0 then
                0.0

            else if initialSubListProbability > 1.0 then
                1.0

            else
                initialSubListProbability

        maxSubListLength =
            3
    in
    Random.weighted ( clampedSubListProbability, True ) [ ( 1 - clampedSubListProbability, False ) ]
        |> Random.andThen
            (\generateSubList ->
                if generateSubList then
                    Random.int 1 maxSubListLength
                        |> Random.andThen
                            (\n ->
                                Random.list n (Random.lazy (\_ -> randomNestedListGenerator (clampedSubListProbability / 2)))
                                    |> Random.map SubList
                            )

                else
                    Random.int 1 100 |> Random.map Elem
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
        [ header [ css headerStyles ]
            [ nav
                [ css
                    [ maxWidth (px 920)
                    , displayFlex
                    , alignItems center
                    , margin2 (px 0) auto
                    , height (pct 100)
                    ]
                ]
                [ logoView
                , h1
                    [ css
                        [ fontSize (px 24)
                        , fontWeight normal
                        , color (hex "#ffffff")
                        ]
                    ]
                    [ a
                        [ css [ textDecoration Css.none, color (hex "#ffffff"), hover [ textDecoration underline ] ]
                        , href "https://github.com/robertlisaru"
                        ]
                        [ text "robertlisaru" ]
                    , span [ css [ margin2 (px 0) (px 10) ] ] [ text "/" ]
                    , a
                        [ css [ textDecoration Css.none, color (hex "#ffffff"), hover [ textDecoration underline ] ]
                        , href "https://github.com/robertlisaru/ninety-nine-elm-problems"
                        ]
                        [ text "ninety-nine-elm-problems" ]
                    ]
                ]
            ]
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


logoView : Html Msg
logoView =
    a
        [ css
            [ textDecoration Css.none
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
        , h2 [ css [ fontSize (px 16), marginTop (px 0), marginBottom (px 50), lineHeight (em 1.5), fontWeight normal ] ] [ text "Sharpen your functional programming skills." ]
        ]


sideBarView : Html Msg
sideBarView =
    div [ css sideBarStyles ]
        [ ul [ css [ listStyleType none, margin (px 0), padding (px 0), color (hex "#1293D8"), fontSize (px 16), lineHeight (em 1.5) ] ]
            [ li [] [ a [] [ text "README" ] ]
            , li [] [ text "About" ]
            , li [] [ text "Source" ]
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Problems" ]
        , input [ placeholder "Search", css searchBarStyles ] []
        , ul [ css [ listStyleType none, margin (px 0), padding (px 0), color (hex "#1293D8"), fontSize (px 16), lineHeight (em 1.5) ] ]
            [ li [] [ text "1. Last element" ]
            , li [] [ text "2. Penultimate" ]
            , li [] [ text "3. Element at" ]
            , li [] [ text "4. Count elements" ]
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
                , p [] [ code [ css codeStyles ] [ text "type NestedList a = Elem a | List [NestedList a]" ] ]
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
                    , value (model.inputStrings |> Array.get problemNumber |> Maybe.withDefault "[]")
                    ]
                    []
                , button
                    [ css [ marginLeft (px 5) ], onClick (RequestRandomList problemNumber) ]
                    [ text "Random" ]
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
                            [ css [ displayFlex, margin4 (px 15) (px 0) (px 15) (px 0) ] ]
                            [ label [ css [ marginRight (px 5) ] ] [ text "Input nested list: " ]
                            , input
                                [ css [ flex (Css.int 1) ]
                                , onInput P7InputUpdate
                                , onBlur P7InputBlur
                                , value model.p7inputString
                                ]
                                []
                            , button
                                [ css [ marginLeft (px 5) ], onClick P7RequestRandomNestedList ]
                                [ text "Random" ]
                            ]
                in
                [ nestedListInput
                , label [] [ text "Flattened list: " ]
                , code [ css codeStyles ]
                    [ text <| (Solutions.P7FlattenNestedList.flatten model.p7nestedList |> Utils.listToString String.fromInt ", ") ]
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
