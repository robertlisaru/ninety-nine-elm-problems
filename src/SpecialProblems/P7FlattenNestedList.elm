module SpecialProblems.P7FlattenNestedList exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import DecoderUtils
import Html.Styled exposing (Html, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P7FlattenNestedList exposing (NestedList(..))
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles, listInputStyles, problemInteractiveAreaStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String, solutionCode : String } -> Model
initModel { problemNumber, problemTitle, solutionCode } =
    let
        nestedList =
            SubList
                [ Elem 1
                , SubList [ SubList [ Elem 2, SubList [ Elem 3, Elem 4 ] ], Elem 5 ]
                , Elem 6
                , SubList [ Elem 7, Elem 8, Elem 9 ]
                ]

        inputString =
            nestedList |> Utils.nestedListToString
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , nestedList = nestedList
    , inputString = inputString
    , showCode = False
    , solutionCode = solutionCode
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , inputString : String
    , showCode : Bool
    , solutionCode : String
    , nestedList : NestedList Int
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (NestedList Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                decodeResult =
                    Decode.decodeString DecoderUtils.nestedListDecoder input

                newNestedList =
                    case decodeResult of
                        Result.Ok nestedList ->
                            nestedList

                        Err _ ->
                            model.nestedList
            in
            ( { model
                | inputString = input
                , nestedList = newNestedList
              }
            , Cmd.none
            )

        UpdateInput ->
            ( { model | inputString = model.nestedList |> Utils.nestedListToString }
            , Cmd.none
            )

        GenerateRandomInput ->
            ( model
            , Random.generate RandomInputReady (RandomUtils.nestedListGenerator 1.0)
            )

        RandomInputReady randomList ->
            ( { model
                | nestedList = randomList
                , inputString = randomList |> Utils.nestedListToString
              }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> Html Msg
specialProblemInteractiveArea model =
    div [ css problemInteractiveAreaStyles ]
        [ div
            [ css inputRowStyles ]
            [ label [ css inputLabelStyles ] [ text "Input nested list: " ]
            , input
                [ css listInputStyles
                , onInput DecodeInput
                , onBlur UpdateInput
                , value model.inputString
                ]
                []
            , niceButton SvgItems.dice "Random" GenerateRandomInput
            ]
        , label [] [ text "Flattened list: " ]
        , code [ css codeStyles ]
            [ text <|
                (Solutions.P7FlattenNestedList.flatten model.nestedList
                    |> Utils.listToString String.fromInt ", "
                )
            ]
        ]
