module SpecialProblems.P3ElementAt exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P3ElementAt
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles, problemInteractiveAreaStyles, secondaryInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String, solutionCode : String } -> Model
initModel { problemNumber, problemTitle, solutionCode } =
    let
        inputList =
            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

        inputString =
            inputList |> Utils.listToString String.fromInt ", "

        index =
            5

        indexString =
            String.fromInt index
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , inputList = inputList
    , inputString = inputString
    , solutionCode = solutionCode
    , index = index
    , indexString = indexString
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , inputList : List Int
    , inputString : String
    , solutionCode : String
    , index : Int
    , indexString : String
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List Int)
    | GenerateRandomIndex
    | RandomIndexReady Int
    | DecodeIndex String
    | UpdateIndex


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
            ( model, Random.generate RandomInputReady (RandomUtils.randomList 10) )

        RandomInputReady randomList ->
            ( { model
                | inputList = randomList
                , inputString = randomList |> Utils.listToString String.fromInt ", "
              }
            , Cmd.none
            )

        GenerateRandomIndex ->
            ( model
            , Random.generate RandomIndexReady
                (Random.int 1 (model.inputList |> List.length))
            )

        RandomIndexReady randomIndex ->
            ( { model
                | index = randomIndex
                , indexString = randomIndex |> String.fromInt
              }
            , Cmd.none
            )

        DecodeIndex input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newIndex =
                    case decodeResult of
                        Result.Ok list ->
                            list

                        Err _ ->
                            model.index
            in
            ( { model | index = newIndex, indexString = input }, Cmd.none )

        UpdateIndex ->
            ( { model | indexString = model.index |> String.fromInt }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> Html Msg
specialProblemInteractiveArea model =
    div [ css problemInteractiveAreaStyles ] <|
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
            [ label [ css inputLabelStyles ] [ text "Index: " ]
            , input
                [ css secondaryInputStyles
                , onInput DecodeIndex
                , onBlur UpdateIndex
                , value model.indexString
                , maxlength 3
                ]
                []
            , niceButton SvgItems.dice "Random" GenerateRandomIndex
            ]
        , label [] [ text "Indexed element is: " ]
        , code [ css codeStyles ]
            [ text <|
                (Solutions.P3ElementAt.elementAt model.index model.inputList
                    |> Utils.maybeToString String.fromInt
                )
            ]
        ]
