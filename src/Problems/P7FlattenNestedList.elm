module Problems.P7FlattenNestedList exposing (Model, Msg, initModel, update, view)

import Css exposing (..)
import DecoderUtils
import Html.Styled exposing (Html, code, div, h3, input, label, li, p, text)
import Html.Styled.Attributes exposing (css, id, value)
import Html.Styled.Events exposing (onBlur, onInput)
import HtmlUtils exposing (niceButton, viewCode)
import Json.Decode as Decode
import Random
import RandomUtils
import Solutions.P7FlattenNestedList exposing (NestedList(..))
import Styles exposing (codeStyles, listInputAreaStyles, listInputStyles, problemInteractiveAreaStyles, problemStyles, problemTitleStyles)
import SvgItems
import Utils


initModel : Int -> String -> String -> Model
initModel problemNumber problemTitle solutionCode =
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
    = ShowCodeToggle
    | DecodeInput String
    | UpdateInput
    | GenerateRandomInput
    | RandomInputReady (NestedList Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCodeToggle ->
            ( { model | showCode = model.showCode |> not }, Cmd.none )

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


view : Model -> Html Msg
view model =
    li
        [ css problemStyles, id (model.problemNumber |> String.fromInt) ]
        [ h3 [ css problemTitleStyles ] [ text <| String.fromInt model.problemNumber ++ ". " ++ model.problemTitle ]
        , p []
            [ text "Flatten a nested lists into a single list. Because Lists in Elm are homogeneous we need to define what a nested list is."
            , viewCode "type NestedList a = Elem a | List [NestedList a]"
            ]
        , div [ css problemInteractiveAreaStyles ]
            [ div
                [ css listInputAreaStyles ]
                [ label [ css [ marginRight (px 5) ] ] [ text "Input nested list: " ]
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
        , niceButton SvgItems.elmColoredLogo
            (if model.showCode then
                "Hide code"

             else
                "Show code (spoiler)"
            )
            ShowCodeToggle
        , Utils.displayIf model.showCode <| (model.solutionCode |> HtmlUtils.viewCode)
        ]
