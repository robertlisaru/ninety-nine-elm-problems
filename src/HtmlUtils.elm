module HtmlUtils exposing (niceButton)

import Css exposing (..)
import Html.Styled exposing (Html, button, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (buttonStyles)


niceButton : Html msg -> String -> msg -> Html msg
niceButton icon label onClickMsg =
    button
        [ css buttonStyles, onClick onClickMsg ]
        [ icon
        , if label /= "" then
            span
                [ css [ marginLeft (em 0.2) ] ]
                [ text label ]

          else
            text ""
        ]
