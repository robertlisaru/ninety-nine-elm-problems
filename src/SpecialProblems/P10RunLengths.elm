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
import Styles exposing (codeLineStyles, inputLabelStyles, inputRowStyles, listInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
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
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , listOfLists : List (List Int)
    , inputString : String
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


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Input duplicates: " ]
        , input
            [ css listInputStyles
            , onInput DecodeInput
            , onBlur UpdateInput
            , value model.inputString
            ]
            []
        , niceButton SvgItems.dice "" GenerateRandomInput
        ]
    , label [ css inputLabelStyles ] [ text "Run lengths: " ]
    , code [ css codeLineStyles ]
        [ text <|
            (Solutions.P10RunLengths.runLengths model.listOfLists
                |> Utils.listToString (Utils.tupleToString String.fromInt String.fromInt) ", "
            )
        ]
    ]
