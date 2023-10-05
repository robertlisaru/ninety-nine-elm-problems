module SpecialProblems.P12RleDecode exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import DecoderUtils
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P12RleDecode
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
    let
        rleCodes =
            [ Run 2 1, Run 3 2 ]

        inputString =
            rleCodes |> Utils.listToString Utils.rleCodeToString ", "
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , rleCodes = rleCodes
    , inputString = inputString
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , rleCodes : List (RleCode Int)
    , inputString : String
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List (RleCode Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                decodeResult =
                    (input |> String.replace "[" "[\"" |> String.replace "]" "\"]" |> String.replace "," "\", \"")
                        |> Decode.decodeString (Decode.list DecoderUtils.rleDecoder)

                decodedInput =
                    case decodeResult of
                        Result.Ok decodedInput_ ->
                            decodedInput_

                        Err _ ->
                            model.rleCodes
            in
            ( { model
                | inputString = input
                , rleCodes = decodedInput
              }
            , Cmd.none
            )

        UpdateInput ->
            ( { model | inputString = model.rleCodes |> Utils.listToString Utils.rleCodeToString ", " }
            , Cmd.none
            )

        GenerateRandomInput ->
            ( model
            , Random.generate RandomInputReady (RandomUtils.duplicateSequences |> Random.map Solutions.P11RleEncode.rleEncode)
            )

        RandomInputReady randomInput ->
            ( { model
                | rleCodes = randomInput
                , inputString = randomInput |> Utils.listToString Utils.rleCodeToString ", "
              }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Input codes: " ]
        , input
            [ css listInputStyles
            , onInput DecodeInput
            , onBlur UpdateInput
            , value model.inputString
            ]
            []
        , niceButton SvgItems.dice "Random" GenerateRandomInput
        ]
    , label [] [ text "Decoded: " ]
    , code [ css codeStyles ]
        [ text <|
            (Solutions.P12RleDecode.rleDecode model.rleCodes
                |> Utils.listToString String.fromInt ", "
            )
        ]
    ]
