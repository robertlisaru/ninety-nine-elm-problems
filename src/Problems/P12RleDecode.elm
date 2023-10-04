module Problems.P12RleDecode exposing (Model, Msg, initModel, update, view)

import Css exposing (..)
import DecoderUtils
import Html.Styled exposing (Html, code, div, h3, input, label, li, p, text)
import Html.Styled.Attributes exposing (css, id, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P12RleDecode
import Styles exposing (codeStyles, listInputAreaStyles, listInputStyles, problemInteractiveAreaStyles, problemStyles, problemTitleStyles)
import SvgItems
import Utils


initModel : Int -> String -> String -> Model
initModel problemNumber problemTitle solutionCode =
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
    , showCode = False
    , solutionCode = solutionCode
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , rleCodes : List (RleCode Int)
    , inputString : String
    , showCode : Bool
    , solutionCode : String
    }


type Msg
    = ShowCodeToggle
    | DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List (RleCode Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCodeToggle ->
            ( { model | showCode = model.showCode |> not }, Cmd.none )

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


view : Model -> Html Msg
view model =
    li
        [ css problemStyles, id (model.problemNumber |> String.fromInt) ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt model.problemNumber ++ ". " ++ model.problemTitle ]
        , p []
            [ text "Decompress the run-length encoded list generated in Problem 11." ]
        , div [ css problemInteractiveAreaStyles ]
            [ div
                [ css listInputAreaStyles ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input codes: " ]
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
        , niceButton SvgItems.elmColoredLogo
            (if model.showCode then
                "Hide code"

             else
                "Show code (spoiler)"
            )
            ShowCodeToggle
        , Utils.displayIf model.showCode <| (model.solutionCode |> HtmlUtils.viewCode)
        ]
