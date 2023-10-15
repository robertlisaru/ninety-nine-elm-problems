module SpecialProblems.P38BenchmarkTotient exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (em, marginRight, marginTop, px, width)
import Html.Styled exposing (Html, button, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Solutions.P34Totient
import Solutions.P37TotientImproved
import Styles exposing (codeStyles, inputLabelStyles, inputRowStyles)
import Task
import Time


test : (Int -> Int) -> Int -> Int
test f n =
    List.range 1 n |> List.map f |> List.length


type alias TestTime =
    { start : Maybe Time.Posix
    , end : Maybe Time.Posix
    }


updateEndTime : TestTime -> Maybe Time.Posix -> TestTime
updateEndTime testTime end =
    { start = testTime.start, end = end }


updateStartTime : TestTime -> Maybe Time.Posix -> TestTime
updateStartTime testTime start =
    { start = start, end = testTime.end }


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
    let
        mRange =
            1000

        inputString =
            mRange |> String.fromInt
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , mRange = mRange
    , inputString = inputString
    , testRepsCompleted = 0
    , testTimeTotient = TestTime Nothing Nothing
    , testTimeTotientImproved = TestTime Nothing Nothing
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , mRange : Int
    , inputString : String
    , testRepsCompleted : Int

    -- don't hard code this or Elm will memoize the test functions
    , testTimeTotient : TestTime
    , testTimeTotientImproved : TestTime
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | StartTest
    | RunTest Int Time.Posix
    | EndTest Int Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                decodeResult =
                    Decode.decodeString Decode.int input

                m =
                    case decodeResult of
                        Result.Ok newInput_ ->
                            newInput_

                        Err _ ->
                            model.mRange
            in
            ( { model
                | inputString = input
                , mRange = m
              }
            , Cmd.none
            )

        UpdateInput ->
            ( { model | inputString = model.mRange |> String.fromInt }
            , Cmd.none
            )

        EndTest n endTime ->
            case n of
                1 ->
                    ( { model | testTimeTotient = updateEndTime model.testTimeTotient (Just endTime) }
                    , Task.perform (RunTest 2) Time.now
                    )

                2 ->
                    ( { model | testTimeTotientImproved = updateEndTime model.testTimeTotient (Just endTime) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RunTest n startTime ->
            ( case n of
                1 ->
                    { model
                        | testTimeTotient = updateStartTime model.testTimeTotient (Just startTime)
                        , testRepsCompleted = test Solutions.P34Totient.totient model.mRange
                    }

                2 ->
                    { model
                        | testTimeTotientImproved = updateEndTime model.testTimeTotient (Just startTime)
                        , testRepsCompleted = test Solutions.P37TotientImproved.totient model.mRange
                    }

                _ ->
                    model
            , Task.perform (EndTest n) Time.now
            )

        StartTest ->
            ( model, Task.perform (RunTest 1) Time.now )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ]
            [ text "Compute totients for all "
            , code [ css codeStyles ] [ text "m" ]
            , text " up to: "
            ]
        , input
            [ css [ width (em 8), marginRight (px 8) ]
            , onInput DecodeInput
            , onBlur UpdateInput
            , value model.inputString
            , maxlength 8
            ]
            []
        ]
    , div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Totients computed: " ]
        , code [ css codeStyles ] [ text (model.testRepsCompleted |> String.fromInt) ]
        ]
    , button [ onClick StartTest ] [ text "Start" ]
    , viewTestTime "Problem 34 algorithm finished in: " model.testTimeTotient
    , viewTestTime "Problem 37 algorithm finished in: " model.testTimeTotientImproved
    ]


viewTestTime : String -> TestTime -> Html msg
viewTestTime label testTime =
    case ( testTime.start, testTime.end ) of
        ( Just start, Just end ) ->
            div [ css [ marginTop (px 15) ] ]
                [ code []
                    [ text label ]
                , code []
                    [ text <|
                        String.fromInt
                            ((end |> Time.posixToMillis)
                                - (start |> Time.posixToMillis)
                            )
                    ]
                , code []
                    [ text " milliseconds." ]
                ]

        ( Just _, Nothing ) ->
            div [] [ text "loading" ]

        _ ->
            text ""
