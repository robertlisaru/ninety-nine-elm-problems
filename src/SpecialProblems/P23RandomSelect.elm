module SpecialProblems.P23RandomSelect exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (marginBottom, marginRight, px)
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P23RandomSelect exposing (randomSelect)
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles, secondaryInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String } -> ( Model, Cmd Msg )
initModel { problemNumber, problemTitle } =
    let
        inputList =
            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

        inputString =
            inputList |> Utils.listToString String.fromInt ", "

        secondaryInput =
            5

        secondaryInputString =
            String.fromInt secondaryInput
    in
    ( { problemNumber = problemNumber
      , problemTitle = problemTitle
      , inputList = inputList
      , inputString = inputString
      , secondaryInput = secondaryInput
      , secondaryInputString = secondaryInputString
      , randomElements = []
      }
    , Random.generate RandomElementsReady <|
        randomSelect secondaryInput inputList
    )


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , inputString : String
    , inputList : List Int
    , secondaryInput : Int
    , secondaryInputString : String
    , randomElements : List Int
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List Int)
    | DecodeBasicSecondaryInput String
    | UpdateBasicSecondaryInput
    | GenerateRandomSecondaryInput
    | RandomSecondaryInputReady Int
    | RandomSelect
    | RandomElementsReady (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                decodeResult =
                    input |> Decode.decodeString (Decode.list Decode.int)

                newInputList =
                    case decodeResult of
                        Result.Ok inputList ->
                            inputList

                        Err _ ->
                            model.inputList
            in
            ( { model
                | inputString = input
                , inputList = newInputList
              }
            , Cmd.none
            )

        UpdateInput ->
            ( { model | inputString = model.inputList |> Utils.listToString String.fromInt ", " }
            , Random.generate RandomElementsReady <|
                randomSelect model.secondaryInput model.inputList
            )

        GenerateRandomInput ->
            ( model
            , Random.generate RandomInputReady (RandomUtils.randomList 10)
            )

        RandomInputReady randomList ->
            ( { model
                | inputList = randomList
                , inputString = randomList |> Utils.listToString String.fromInt ", "
              }
            , Random.generate RandomElementsReady <|
                randomSelect model.secondaryInput randomList
            )

        GenerateRandomSecondaryInput ->
            ( model, Random.generate RandomSecondaryInputReady (Random.int 0 10) )

        RandomSecondaryInputReady randomInt ->
            ( { model
                | secondaryInput = randomInt
                , secondaryInputString = randomInt |> String.fromInt
              }
            , Random.generate RandomElementsReady <|
                randomSelect randomInt model.inputList
            )

        DecodeBasicSecondaryInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.secondaryInput
            in
            ( { model
                | secondaryInputString = input
                , secondaryInput = newInt
              }
            , Random.generate RandomElementsReady <|
                randomSelect newInt model.inputList
            )

        UpdateBasicSecondaryInput ->
            ( { model | secondaryInputString = model.secondaryInput |> String.fromInt }
            , Cmd.none
            )

        RandomSelect ->
            ( model
            , Random.generate RandomElementsReady <|
                randomSelect model.secondaryInput model.inputList
            )

        RandomElementsReady randomElements ->
            ( { model | randomElements = randomElements }, Cmd.none )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Input list: " ]
        , input
            [ css listInputStyles
            , onInput DecodeInput
            , onBlur UpdateInput
            , value model.inputString
            ]
            []
        , niceButton SvgItems.dice "Random" GenerateRandomInput
        ]
    , div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "n: " ]
        , input
            [ css secondaryInputStyles
            , onInput DecodeBasicSecondaryInput
            , onBlur UpdateBasicSecondaryInput
            , value model.secondaryInputString
            , maxlength 3
            ]
            []
        , niceButton SvgItems.dice "Random" GenerateRandomSecondaryInput
        ]
    , div [ css (inputRowStyles ++ [ marginBottom (px 0) ]) ]
        [ label [] [ text "Random elements: " ]
        , code [ css (codeStyles ++ [ marginRight (px 8) ]) ]
            [ text (model.randomElements |> Utils.listToString String.fromInt ", ") ]
        , niceButton SvgItems.dice "Random" RandomSelect
        ]
    ]
