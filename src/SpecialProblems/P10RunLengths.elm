module SpecialProblems.P10RunLengths exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import DecoderUtils
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P10RunLengths
import Solutions.P9Pack
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles, problemInteractiveAreaStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String, solutionCode : String } -> Model
initModel { problemNumber, problemTitle, solutionCode } =
    let
        listOfLists =
            [ [ 1, 1 ], [ 2, 2, 2 ] ]

        inputString =
            listOfLists |> Utils.listOfListsToString
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , listOfLists = listOfLists
    , inputString = inputString
    , showCode = False
    , solutionCode = solutionCode
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , listOfLists : List (List Int)
    , inputString : String
    , showCode : Bool
    , solutionCode : String
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List (List Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                decodeResult =
                    Decode.decodeString DecoderUtils.decodeDuplicates input

                listOfLists =
                    case decodeResult of
                        Result.Ok newInput_ ->
                            newInput_

                        Err _ ->
                            model.listOfLists
            in
            ( { model
                | inputString = input
                , listOfLists = listOfLists
              }
            , Cmd.none
            )

        UpdateInput ->
            ( { model | inputString = model.listOfLists |> Utils.listOfListsToString }
            , Cmd.none
            )

        GenerateRandomInput ->
            ( model
            , Random.generate RandomInputReady (RandomUtils.duplicateSequences |> Random.map Solutions.P9Pack.pack)
            )

        RandomInputReady randomListOfLists ->
            ( { model
                | listOfLists = randomListOfLists
                , inputString = randomListOfLists |> Utils.listOfListsToString
              }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> Html Msg
specialProblemInteractiveArea model =
    div [ css problemInteractiveAreaStyles ]
        [ div [ css inputRowStyles ]
            [ label [ css inputLabelStyles ] [ text "Input duplicates: " ]
            , input
                [ css listInputStyles
                , onInput DecodeInput
                , onBlur UpdateInput
                , value model.inputString
                ]
                []
            , niceButton SvgItems.dice "Random" GenerateRandomInput
            ]
        , label [] [ text "Run lengths: " ]
        , code [ css codeStyles ]
            [ text <|
                (Solutions.P10RunLengths.runLengths model.listOfLists
                    |> Utils.listToString Utils.tupleToString ", "
                )
            ]
        ]
