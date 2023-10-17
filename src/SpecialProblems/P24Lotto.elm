module SpecialProblems.P24Lotto exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (marginBottom, marginLeft, marginRight, px)
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import Solutions.P24Lotto exposing (lotto)
import Styles exposing (codeLineStyles, inputLabelStyles, inputRowStyles, secondaryInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String } -> ( Model, Cmd Msg )
initModel { problemNumber, problemTitle } =
    let
        start =
            6

        startInputString =
            String.fromInt start

        end =
            49

        endInputString =
            String.fromInt end

        n =
            6

        nInputString =
            String.fromInt n
    in
    ( { problemNumber = problemNumber
      , problemTitle = problemTitle
      , start = start
      , startInputString = startInputString
      , end = end
      , endInputString = endInputString
      , n = n
      , nInputString = nInputString
      , randomElements = []
      }
    , Random.generate LottoReady <| lotto n start end
    )


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , start : Int
    , startInputString : String
    , end : Int
    , endInputString : String
    , n : Int
    , nInputString : String
    , randomElements : List Int
    }


type Msg
    = DecodeStartInput String
    | UpdateStartInput
    | GenerateStartInput
    | RandomStartInputReady Int
    | DecodeEndInput String
    | UpdateEndInput
    | GenerateEndInput
    | UpdateNInput
    | DecodeNInput String
    | RandomEndInputReady Int
    | GenerateNInput
    | RandomNInputReady Int
    | Lotto
    | LottoReady (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateStartInput ->
            ( model, Random.generate RandomStartInputReady (Random.int 0 48) )

        RandomStartInputReady randomInt ->
            ( { model
                | start = randomInt
                , startInputString = randomInt |> String.fromInt
              }
            , Random.generate LottoReady <|
                lotto model.n randomInt model.end
            )

        DecodeStartInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.start
            in
            ( { model
                | startInputString = input
                , start = newInt
              }
            , Random.generate LottoReady <|
                lotto model.n newInt model.end
            )

        UpdateStartInput ->
            ( { model | startInputString = model.start |> String.fromInt }
            , Cmd.none
            )

        DecodeEndInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.start
            in
            ( { model
                | endInputString = input
                , end = newInt
              }
            , Random.generate LottoReady <|
                lotto model.n model.start newInt
            )

        UpdateEndInput ->
            ( { model | endInputString = model.end |> String.fromInt }
            , Cmd.none
            )

        GenerateEndInput ->
            ( model, Random.generate RandomEndInputReady (Random.int 49 100) )

        RandomEndInputReady randomInt ->
            ( { model
                | end = randomInt
                , endInputString = randomInt |> String.fromInt
              }
            , Random.generate LottoReady <|
                lotto model.n model.start randomInt
            )

        GenerateNInput ->
            ( model, Random.generate RandomNInputReady (Random.int 0 10) )

        RandomNInputReady randomInt ->
            ( { model
                | n = randomInt
                , nInputString = randomInt |> String.fromInt
              }
            , Random.generate LottoReady <|
                lotto randomInt model.start model.end
            )

        UpdateNInput ->
            ( { model | nInputString = model.n |> String.fromInt }
            , Cmd.none
            )

        DecodeNInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                newInt =
                    case decodeResult of
                        Result.Ok decodedInt ->
                            decodedInt

                        Err _ ->
                            model.start
            in
            ( { model
                | nInputString = input
                , n = newInt
              }
            , Random.generate LottoReady <|
                lotto newInt model.start model.end
            )

        Lotto ->
            ( model
            , Random.generate LottoReady <| lotto model.n model.start model.end
            )

        LottoReady randomElements ->
            ( { model | randomElements = randomElements }, Cmd.none )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Start: " ]
        , input
            [ css secondaryInputStyles
            , onInput DecodeStartInput
            , onBlur UpdateStartInput
            , value model.startInputString
            , maxlength 3
            ]
            []
        , niceButton SvgItems.dice "" GenerateStartInput
        , label [ css (inputLabelStyles ++ [ marginLeft (px 24) ]) ] [ text "End: " ]
        , input
            [ css secondaryInputStyles
            , onInput DecodeEndInput
            , onBlur UpdateEndInput
            , value model.endInputString
            , maxlength 3
            ]
            []
        , niceButton SvgItems.dice "" GenerateEndInput
        ]
    , div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "n: " ]
        , input
            [ css secondaryInputStyles
            , onInput DecodeNInput
            , onBlur UpdateNInput
            , value model.nInputString
            , maxlength 3
            ]
            []
        , niceButton SvgItems.dice "" GenerateNInput
        ]
    , div [ css (inputRowStyles ++ [ marginBottom (px 0) ]) ]
        [ label [ css inputLabelStyles ] [ text "Random elements: " ]
        , code [ css (codeLineStyles ++ [ marginRight (px 8) ]) ]
            [ text (model.randomElements |> Utils.listToString String.fromInt ", ") ]
        , niceButton SvgItems.dice "" Lotto
        ]
    ]
