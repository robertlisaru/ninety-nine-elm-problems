module Main exposing (Model, Msg(..), main)

import Browser exposing (Document)
import Css exposing (alignItems, auto, backgroundColor, borderColor, borderRadius, borderStyle, borderWidth, boxShadow4, center, column, displayFlex, flexDirection, fontFamilies, listStyleType, marginBottom, marginLeft, marginRight, marginTop, maxWidth, minHeight, minWidth, none, padding, pct, px, rgb, rgba, solid, width)
import Html.Styled exposing (Html, button, li, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Problems exposing (Problem, problems)
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always ( Model [], Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { randomList : List Int
    }



-- UPDATE


type Msg
    = RequestRandomList
    | RandomListArrived (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestRandomList ->
            ( model, Random.generate RandomListArrived (Random.list 10 (Random.int 1 100)) )

        RandomListArrived randomList ->
            ( { model | randomList = randomList }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Ninety-nine Elm solutions"
    , body =
        [ ul
            [ css
                [ listStyleType none
                , padding (px 0)
                , marginTop (px 25)
                , marginLeft auto
                , marginRight auto
                , width (pct 100)
                , displayFlex
                , flexDirection column
                , alignItems center
                , fontFamilies [ "arial", "sans-serif" ]
                ]
            ]
            (problems |> List.map (viewProblem model))
        ]
            |> List.map toUnstyled
    }


viewProblem : Model -> Problem -> Html Msg
viewProblem model { identifier, title } =
    case identifier of
        "23" ->
            problem23 model

        _ ->
            li
                [ css problemStyles ]
                [ text <| identifier ++ ". " ++ title ]


problem23 : Model -> Html Msg
problem23 model =
    li [ css problemStyles ]
        [ text ("23. " ++ "Random list elements")
        , button [ onClick RequestRandomList ] [ text "Generate list" ]
        , span [] (model.randomList |> List.map (\number -> text (String.fromInt number ++ " ")))
        ]


problemStyles : List Css.Style
problemStyles =
    [ borderWidth (px 1)
    , borderStyle solid
    , borderColor (rgb 150 150 150)
    , borderRadius (px 15)
    , boxShadow4 (px 5) (px 5) (px 5) (rgba 0 0 0 0.5)
    , marginBottom (px 25)
    , maxWidth (px 775)
    , minWidth (px 300)
    , width (pct 80)
    , minHeight (px 300)
    , padding (px 25)
    , backgroundColor (rgb 200 255 255)
    ]
