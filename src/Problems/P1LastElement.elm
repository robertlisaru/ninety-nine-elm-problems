module Problems.P1LastElement exposing (Model, Msg, initModel, update, view)

import Css exposing (..)
import Html.Styled exposing (Html, code, div, h3, input, label, li, p, text)
import Html.Styled.Attributes exposing (css, id, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P1LastElement
import Styles exposing (codeStyles, listInputAreaStyles, listInputStyles, problemInteractiveAreaStyles, problemStyles, problemTitleStyles)
import SvgItems
import Utils


initModel : Int -> String -> String -> Model
initModel problemNumber problemTitle solutionCode =
    let
        inputList =
            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

        inputString =
            inputList |> Utils.listToString String.fromInt ", "
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , inputList = inputList
    , inputString = inputString
    , showCode = False
    , solutionCode = solutionCode
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , inputList : List Int
    , inputString : String
    , showCode : Bool
    , solutionCode : String
    }


type Msg
    = ShowCodeToggle
    | DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCodeToggle ->
            ( { model | showCode = model.showCode |> not }, Cmd.none )

        DecodeInput input ->
            let
                decodeResult =
                    Decode.decodeString (Decode.list Decode.int) input

                newList =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.inputList
            in
            ( { model | inputList = newList, inputString = input }, Cmd.none )

        UpdateInput ->
            ( { model
                | inputString =
                    model.inputList |> Utils.listToString String.fromInt ", "
              }
            , Cmd.none
            )

        GenerateRandomInput ->
            ( model, Random.generate RandomInputReady (RandomUtils.randomList 10) )

        RandomInputReady randomList ->
            ( { model
                | inputList = randomList
                , inputString = randomList |> Utils.listToString String.fromInt ", "
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    li
        [ css problemStyles, id (model.problemNumber |> String.fromInt) ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt model.problemNumber ++ ". " ++ model.problemTitle ]
        , p []
            [ text "Write a function "
            , code [ css codeStyles ] [ text "last" ]
            , text " that returns the last element of a list. An empty list doesn't have a last element, therefore "
            , code [ css codeStyles ] [ text "last" ]
            , text " must return a "
            , code [ css codeStyles ] [ text "Maybe" ]
            , text "."
            ]
        , div [ css problemInteractiveAreaStyles ] <|
            [ div
                [ css listInputAreaStyles ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input list: " ]
                , input
                    [ css listInputStyles
                    , onInput DecodeInput
                    , onBlur UpdateInput
                    , value model.inputString
                    ]
                    []
                , niceButton SvgItems.dice "Random" GenerateRandomInput
                ]
            , label [] [ text "Last element is: " ]
            , code [ css codeStyles ]
                [ text <|
                    (Solutions.P1LastElement.last model.inputList
                        |> Utils.maybeToString String.fromInt
                    )
                ]
            ]
        , niceButton SvgItems.elmColoredLogo
            (if model.showCode then
                "Hide code"

             else
                "Show code (spoiler)"
            )
            ShowCodeToggle
        , Utils.displayIf model.showCode <| (model.solutionCode |> HtmlUtils.viewCode)
        ]
