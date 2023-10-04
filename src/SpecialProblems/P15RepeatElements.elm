module SpecialProblems.P15RepeatElements exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (..)
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P15RepeatElements
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles, problemInteractiveAreaStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String, solutionCode : String } -> Model
initModel { problemNumber, problemTitle, solutionCode } =
    let
        inputList =
            [ 1, 2, 3 ]

        inputString =
            inputList |> Utils.listToString String.fromInt ", "

        secondaryInput =
            3

        secondaryInputString =
            String.fromInt secondaryInput
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , inputList = inputList
    , inputString = inputString
    , showCode = False
    , solutionCode = solutionCode
    , secondaryInput = secondaryInput
    , secondaryInputString = secondaryInputString
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , inputList : List Int
    , inputString : String
    , showCode : Bool
    , solutionCode : String
    , secondaryInput : Int
    , secondaryInputString : String
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List Int)
    | GenerateRandomSecondaryInput
    | RandomSecondaryInputReady Int
    | DecodeSecondaryInput String
    | UpdateSecondaryInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( model, Random.generate RandomInputReady (RandomUtils.randomList 5) )

        RandomInputReady randomList ->
            ( { model
                | inputList = randomList
                , inputString = randomList |> Utils.listToString String.fromInt ", "
              }
            , Cmd.none
            )

        GenerateRandomSecondaryInput ->
            ( model, Random.generate RandomSecondaryInputReady (Random.int 0 3) )

        RandomSecondaryInputReady secondaryInput ->
            ( { model
                | secondaryInput = secondaryInput
                , secondaryInputString = secondaryInput |> String.fromInt
              }
            , Cmd.none
            )

        DecodeSecondaryInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newSecondaryInput =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.secondaryInput
            in
            ( { model | secondaryInput = newSecondaryInput, secondaryInputString = input }, Cmd.none )

        UpdateSecondaryInput ->
            ( { model | secondaryInputString = model.secondaryInput |> String.fromInt }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> Html Msg
specialProblemInteractiveArea model =
    div [ css problemInteractiveAreaStyles ] <|
        [ div
            [ css inputRowStyles ]
            [ label [ css inputLabelStyles ] [ text "Input list: " ]
            , input [ css listInputStyles, onInput DecodeInput, onBlur UpdateInput, value model.inputString ] []
            , niceButton SvgItems.dice "Random" GenerateRandomInput
            ]
        , div
            [ css inputRowStyles ]
            [ label [ css inputLabelStyles ] [ text "Repeat times: " ]
            , input
                [ css [ width (em 3), marginRight (px 8) ]
                , onInput DecodeSecondaryInput
                , onBlur UpdateSecondaryInput
                , value model.secondaryInputString
                , maxlength 3
                ]
                []
            , niceButton SvgItems.dice "Random" GenerateRandomSecondaryInput
            ]
        , label [] [ text "Repeated elements: " ]
        , code [ css codeStyles ]
            [ text <|
                (Solutions.P15RepeatElements.repeatElements model.secondaryInput model.inputList
                    |> Utils.listToString String.fromInt ", "
                )
            ]
        ]
