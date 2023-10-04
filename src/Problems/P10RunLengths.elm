module Problems.P10RunLengths exposing (Model, Msg, initModel, update, view)

import Css exposing (..)
import DecoderUtils
import Html.Styled exposing (Html, code, div, h3, input, label, li, p, text)
import Html.Styled.Attributes exposing (css, id, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P10RunLengths
import Solutions.P9Pack
import Styles exposing (codeStyles, listInputAreaStyles, listInputStyles, problemInteractiveAreaStyles, problemStyles, problemTitleStyles)
import SvgItems
import Utils


initModel : Int -> String -> String -> Model
initModel problemNumber problemTitle solutionCode =
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
    = ShowCodeToggle
    | DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (List (List Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCodeToggle ->
            ( { model | showCode = model.showCode |> not }, Cmd.none )

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


view : Model -> Html Msg
view model =
    li
        [ css problemStyles, id (model.problemNumber |> String.fromInt) ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt model.problemNumber ++ ". " ++ model.problemTitle ]
        , p []
            [ text "Run-length encode a list of list to a list of tuples. Unlike lists, tuples can mix types. Use tuples (n, e) to encode a list where n is the number of duplicates of the element e." ]
        , div [ css problemInteractiveAreaStyles ]
            [ div
                [ css listInputAreaStyles ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input duplicates: " ]
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
        , niceButton SvgItems.elmColoredLogo
            (if model.showCode then
                "Hide code"

             else
                "Show code (spoiler)"
            )
            ShowCodeToggle
        , Utils.displayIf model.showCode <| (model.solutionCode |> HtmlUtils.viewCode)
        ]
