module HtmlUtils exposing (niceButton, viewCode)

import Css exposing (..)
import Html.Styled exposing (Html, button, code, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (buttonStyles)
import SyntaxHighlight


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


viewCode : String -> Html msg
viewCode solutionCode =
    div [ css [ marginTop (px 15) ] ]
        [ SyntaxHighlight.elm solutionCode
            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
            |> Result.map fromUnstyled
            |> Result.withDefault (code [] [ text "Syntax highlight error." ])
        ]
