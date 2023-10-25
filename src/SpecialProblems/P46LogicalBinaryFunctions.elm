module SpecialProblems.P46LogicalBinaryFunctions exposing (Model, Msg, initModel, specialProblemInteractiveArea, update)

import Css exposing (bold, color, fontWeight, hex)
import Html.Styled exposing (Html, div, label, option, select, table, td, text, tr)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Solutions.P46LogicalBinaryFunctions
import Styles exposing (inputLabelStyles, inputRowStyles)
import Utils exposing (boolToString)


initModel : { problemNumber : Int, problemTitle : String } -> Model
initModel { problemNumber, problemTitle } =
    { problemNumber = problemNumber
    , problemTitle = problemTitle
    , logicalBinaryFunction = AND
    }


type LogicalBinaryFunction
    = AND
    | OR
    | NAND
    | NOR
    | XOR
    | IMPLIES
    | EQUIVALENT
    | NONE


logicalBinaryFunctionToString : LogicalBinaryFunction -> String
logicalBinaryFunctionToString l =
    case l of
        AND ->
            "AND"

        OR ->
            "OR"

        NAND ->
            "NAND"

        NOR ->
            "NOR"

        XOR ->
            "XOR"

        IMPLIES ->
            "IMPLIES"

        EQUIVALENT ->
            "EQUIVALENT"

        NONE ->
            ""


stringToLogicalBinaryFunction : String -> LogicalBinaryFunction
stringToLogicalBinaryFunction string =
    case string of
        "AND" ->
            AND

        "OR" ->
            OR

        "NAND" ->
            NAND

        "NOR" ->
            NOR

        "XOR" ->
            XOR

        "IMPLIES" ->
            IMPLIES

        "EQUIVALENT" ->
            EQUIVALENT

        _ ->
            NONE


type alias Model =
    { problemNumber : Int
    , problemTitle : String
    , logicalBinaryFunction : LogicalBinaryFunction
    }


type Msg
    = DecodeInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DecodeInput input ->
            let
                logicalBinaryFunction =
                    stringToLogicalBinaryFunction input
            in
            ( { model
                | logicalBinaryFunction = logicalBinaryFunction
              }
            , Cmd.none
            )


specialProblemInteractiveArea : Model -> List (Html Msg)
specialProblemInteractiveArea model =
    [ div [ css inputRowStyles ]
        [ label [ css inputLabelStyles ] [ text "Logical function: " ]
        , select [ onInput DecodeInput ]
            [ option [ value "AND" ] [ text "AND" ]
            , option [ value "OR" ] [ text "OR" ]
            , option [ value "NAND" ] [ text "NAND" ]
            , option [ value "NOR" ] [ text "NOR" ]
            , option [ value "XOR" ] [ text "XOR" ]
            , option [ value "IMPLIES" ] [ text "IMPLIES" ]
            , option [ value "EQUIVALENT" ] [ text "EQUIVALENT" ]
            , option [ value "NONE" ] [ text "NONE" ]
            ]
        ]
    , Utils.displayIf (model.logicalBinaryFunction /= NONE) <| label [ css inputLabelStyles ] [ text "Truth table: " ]
    , Utils.displayIf (model.logicalBinaryFunction /= NONE) <| viewTruthTable model.logicalBinaryFunction
    ]


viewTruthTable : LogicalBinaryFunction -> Html Msg
viewTruthTable lbf =
    let
        truthTable =
            case lbf of
                AND ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.and

                OR ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.or

                NAND ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.nand

                NOR ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.nor

                XOR ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.xor

                IMPLIES ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.implies

                EQUIVALENT ->
                    Solutions.P46LogicalBinaryFunctions.truthTable Solutions.P46LogicalBinaryFunctions.equivalent

                NONE ->
                    []
    in
    table [] <|
        tr [ css [ fontWeight bold, color (hex "#596277") ] ]
            [ td [] [ text "A" ]
            , td [] [ text "B" ]
            , td [] [ text ("A " ++ logicalBinaryFunctionToString lbf ++ " B") ]
            ]
            :: (truthTable
                    |> List.map
                        (\truthTableRow ->
                            case truthTableRow of
                                ( a, b, c ) ->
                                    tr []
                                        [ td [ css [ color <| boolColor a ] ] [ text <| boolToString <| a ]
                                        , td [ css [ color <| boolColor b ] ] [ text <| boolToString <| b ]
                                        , td [ css [ color <| boolColor c ] ] [ text <| boolToString <| c ]
                                        ]
                        )
               )


boolColor : Bool -> Css.Color
boolColor b =
    if b then
        hex "#8CD636"

    else
        hex "#EEA400"
