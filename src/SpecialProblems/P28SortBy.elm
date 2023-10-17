module SpecialProblems.P28SortBy exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import DecoderUtils
import Html.Styled exposing (Html, code, div, input, label, option, select, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P28SortBy
import Solutions.P9Pack
import Styles exposing (codeLineStyles, inputLabelStyles, inputRowStyles, listInputStyles)
import SvgItems
import Utils


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
    let
        listOfLists =
            [ [ 8 ], [ 9 ], [ 4, 5 ], [ 1, 2, 3 ], [ 6, 7 ], [ 10 ] ]

        inputString =
            listOfLists |> Utils.listOfListsToString
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , listOfLists = listOfLists
    , inputString = inputString
    , sortByLengthFrequency = False
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , listOfLists : List (List Int)
    , inputString : String
    , sortByLengthFrequency : Bool
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List (List Int))
    | SortByListLengths
    | SortByLengthFrequency


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

        SortByListLengths ->
            ( { model | sortByLengthFrequency = False }, Cmd.none )

        SortByLengthFrequency ->
            ( { model | sortByLengthFrequency = True }, Cmd.none )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Input lists: " ]
        , input
            [ css listInputStyles
            , onInput DecodeInput
            , onBlur UpdateInput
            , value model.inputString
            ]
            []
        , niceButton SvgItems.dice "" GenerateRandomInput
        ]
    , div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Sort by: " ]
        , select []
            [ option [ onClick SortByListLengths ] [ text "List lengths" ]
            , option [ onClick SortByLengthFrequency ] [ text "Length frequency" ]
            ]
        ]
    , label [ css inputLabelStyles ] [ text "Sorted: " ]
    , code [ css codeLineStyles ]
        [ text <|
            if model.sortByLengthFrequency then
                Solutions.P28SortBy.sortByLengthFrequency model.listOfLists
                    |> Utils.listOfListsToString

            else
                Solutions.P28SortBy.sortByListLengths model.listOfLists
                    |> Utils.listOfListsToString
        ]
    ]
