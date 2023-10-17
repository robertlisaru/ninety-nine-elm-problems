module SpecialProblems.P38BenchmarkTotient exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (em, marginRight, marginTop, px, width)
import Html.Styled exposing (Html, button, code, div, input, label, text)
import Html.Styled.Attributes exposing (css, maxlength, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Process
import Solutions.P34Totient
import Solutions.P37TotientImproved
import Styles exposing (codeLineStyles, inputLabelStyles, inputRowStyles)
import Task
import Time


test : (Int -> Int) -> Int -> Int
test f n =
    List.range 1 n |> List.map f |> List.length


type alias TestTime =
    { start : Time.Posix
    , end : Time.Posix
    }


type TestStatus
    = NotStarted
    | Started
    | Completed Int TestTime


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
    let
        mRange =
            10000

        inputString =
            mRange |> String.fromInt
    in
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , mRange = mRange
    , inputString = inputString
    , testTotient1 = NotStarted
    , testTotient2 = NotStarted
    }


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , mRange : Int
    , inputString : String
    , testTotient1 : TestStatus
    , testTotient2 : TestStatus
    }


type Msg
    = DecodeInput String
    | UpdateInput
    | StartTest Int
    | RunTest Int Time.Posix
    | EndTest Int Int Time.Posix Time.Posix


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

        EndTest n totientsComputed startTime endTime ->
            case n of
                1 ->
                    ( { model | testTotient1 = Completed totientsComputed (TestTime startTime endTime) }
                    , Task.perform (always (StartTest 2)) Time.now
                    )

                2 ->
                    ( { model | testTotient2 = Completed totientsComputed (TestTime startTime endTime) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RunTest n startTime ->
            let
                totientsComputed =
                    case n of
                        1 ->
                            test Solutions.P34Totient.totient model.mRange

                        2 ->
                            test Solutions.P37TotientImproved.totient model.mRange

                        _ ->
                            0
            in
            ( model
            , Task.perform (EndTest n totientsComputed startTime) Time.now
            )

        StartTest n ->
            ( case n of
                1 ->
                    { model | testTotient1 = Started }

                2 ->
                    { model | testTotient2 = Started }

                _ ->
                    model
            , Process.sleep 20 |> Task.andThen (always Time.now) |> Task.perform (RunTest n)
            )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ]
            [ text "Compute totients for all "
            , code [ css codeLineStyles ] [ text "m" ]
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
    , button [ onClick (StartTest 1) ] [ text "Start" ]
    , viewTestStatus "Problem 34: " model.testTotient1
    , viewTestStatus "Problem 37: " model.testTotient2
    ]


viewTestStatus : String -> TestStatus -> Html Msg
viewTestStatus label testStatus =
    div [ css [ marginTop (px 15) ] ] <|
        code []
            [ text label ]
            :: (case testStatus of
                    Completed totientsComputed testTime ->
                        [ code []
                            [ text "computed " ]
                        , code [ css codeLineStyles ]
                            [ text <| String.fromInt totientsComputed ]
                        , code []
                            [ text " totients in " ]
                        , code [ css codeLineStyles ]
                            [ text <|
                                String.fromInt
                                    ((testTime.end |> Time.posixToMillis)
                                        - (testTime.start |> Time.posixToMillis)
                                    )
                            ]
                        , code []
                            [ text " milliseconds." ]
                        ]

                    Started ->
                        [ code []
                            [ text "computing totients..." ]
                        ]

                    NotStarted ->
                        [ code []
                            [ text "not started" ]
                        ]
               )
