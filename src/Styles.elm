module Styles exposing
    ( buttonStyles
    , codeBlockStyles
    , codeLineStyles
    , disabledBackground
    , genericStylesNode
    , hamburgerButtonStyles
    , headerStyles
    , inputLabelStyles
    , inputRowStyles
    , leftContentStyles
    , linkStyles
    , listInputStyles
    , mainHeadingStyles
    , navStyles
    , pageContainerStyles
    , problemInteractiveAreaStyles
    , problemListStyles
    , problemStyles
    , problemSubTitleStyles
    , problemTitleStyles
    , searchBarStyles
    , secondaryInputStyles
    , sideBarItemListStyles
    , sideBarStyles
    , subHeadingStyles
    , syntaxHighlightRequiredCssNode
    , syntaxHighlightThemeCssNode
    )

import Css exposing (..)
import Css.Global exposing (global, selector)
import Css.Transitions exposing (transition)
import DeviceType exposing (DeviceType(..))
import Html.Styled exposing (Html, div, node, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


headerStyles : DeviceType -> List Style
headerStyles deviceType =
    case deviceType of
        Mobile ->
            [ backgroundColor (hex "#5FABDC")
            , color (hex "#ffffff")
            , paddingLeft (px 8)
            , paddingRight (px 8)
            , overflowX hidden
            , position fixed
            , top (px 0)
            , height (px 50)
            , width (calc (pct 100) minus (px 16))
            , boxShadow5 (px 0) (px 3) (px 6) (px 3) (rgba 0 0 0 0.5)
            , zIndex (int 2)
            ]

        Desktop ->
            [ height (px 64)
            , backgroundColor (hex "#5FABDC")
            , color (hex "#ffffff")
            , paddingLeft (px 40)
            , paddingRight (px 40)
            , overflowX hidden
            ]


navStyles : List Style
navStyles =
    [ maxWidth (px 920)
    , displayFlex
    , alignItems center
    , margin2 (px 0) auto
    , height (pct 100)
    ]


pageContainerStyles : DeviceType -> List Style
pageContainerStyles deviceType =
    case deviceType of
        Mobile ->
            [ justifyContent center
            , marginTop (px 70)
            , paddingLeft (px 8)
            , paddingRight (px 8)
            ]

        Desktop ->
            [ displayFlex
            , justifyContent center
            , width (pct 100)
            ]


mainHeadingStyles : DeviceType -> List Style
mainHeadingStyles deviceType =
    case deviceType of
        Mobile ->
            [ fontSize (em 2)
            , marginBottom (px 0)
            , fontWeight normal
            ]

        Desktop ->
            [ fontSize (em 3)
            , marginBottom (px 0)
            , fontWeight normal
            ]


subHeadingStyles : DeviceType -> List Style
subHeadingStyles deviceType =
    case deviceType of
        Mobile ->
            [ fontSize (px 16)
            , marginTop (px 0)
            , marginBottom (px 30)
            , lineHeight (em 1.5)
            , fontWeight normal
            ]

        Desktop ->
            [ fontSize (px 16)
            , marginTop (px 0)
            , marginBottom (px 50)
            , lineHeight (em 1.5)
            , fontWeight normal
            ]


leftContentStyles : DeviceType -> List Style
leftContentStyles deviceType =
    case deviceType of
        Mobile ->
            []

        Desktop ->
            [ maxWidth (px 600)
            , marginLeft (px 40)
            ]


sideBarStyles : Bool -> DeviceType -> List Style
sideBarStyles isOpen deviceType =
    case deviceType of
        Mobile ->
            [ position fixed
            , top (px 50)
            , right (px 0)
            , zIndex (int 2)
            , property "height" "calc(100svh - 66px)"
            , width (calc (vw 100) minus (em 5))
            , boxShadow5 (px -3) (px 8) (px 6) (px 3) (rgba 0 0 0 0.5)
            , borderRadius4 (px 8) (px 0) (px 0) (px 8)
            , padding (px 8)
            , paddingLeft (px 16)
            , backgroundColor (hex "#fff")
            , overflowY scroll
            , transition
                [ Css.Transitions.transform 200
                ]
            ]
                ++ (if isOpen then
                        [ transform (translateY (vw 0))
                        ]

                    else
                        [ transform (translateX (vw 100))
                        ]
                   )

        Desktop ->
            [ maxWidth (px 260)
            , padding (px 20)
            , paddingRight (px 0)
            , margin4 (px 20) (px 40) (px 20) (px 40)
            , borderLeft3 (px 1) solid (hex "#eeeeee")
            ]


searchBarStyles : List Style
searchBarStyles =
    [ fontSize (em 1)
    , padding (px 4)
    , margin2 (px 10) (px 0)
    , border3 (px 1) solid (hex "#eeeeee")
    , borderRadius (px 6)
    , width (pct 100)
    ]


problemListStyles : List Style
problemListStyles =
    [ listStyleType none
    , padding (px 0)
    , marginTop (px 25)
    ]


problemTitleStyles : DeviceType -> List Style
problemTitleStyles deviceType =
    case deviceType of
        Mobile ->
            [ color (hex "#1293D8")
            , textDecoration none
            , fontSize (em 1.5)
            , fontWeight normal
            , displayFlex
            , alignItems center
            , marginTop (px 8)
            ]

        Desktop ->
            [ color (hex "#1293D8")
            , textDecoration none
            , fontSize (em 1.5)
            , fontWeight normal
            , displayFlex
            , alignItems center
            ]


problemSubTitleStyles : List Style
problemSubTitleStyles =
    [ color (hex "#1293D8")
    , textDecoration none
    , fontSize (em 1.5)
    , fontWeight normal
    , displayFlex
    , alignItems center
    , margin (px 0)
    ]


problemStyles : DeviceType -> List Style
problemStyles deviceType =
    case deviceType of
        Mobile ->
            [ borderWidth (px 1)
            , borderStyle solid
            , borderColor (hex "#f5f5f5")
            , borderRadius (px 6)
            , marginBottom (px 50)
            , padding (px 8)
            , paddingTop (px 0)
            ]

        Desktop ->
            [ borderWidth (px 1)
            , borderStyle solid
            , borderColor (hex "#f5f5f5")
            , borderRadius (px 6)
            , hover
                [ boxShadow4 (px 3) (px 3) (px 3) (rgba 0 0 0 0.1)
                , borderColor (hex "#e0e0e0")
                ]
            , marginBottom (px 50)
            , padding (px 25)
            , paddingTop (px 0)
            , property "scroll-margin-top" "25px"
            ]


problemInteractiveAreaStyles : DeviceType -> List Style
problemInteractiveAreaStyles deviceType =
    case deviceType of
        Mobile ->
            [ borderWidth (px 1)
            , borderStyle solid
            , borderColor (hex "#f5f5f5")
            , borderRadius (px 6)
            , padding (px 8)
            , backgroundColor (hex "#fdfdfd")
            , marginBottom (px 15)
            ]

        Desktop ->
            [ borderWidth (px 1)
            , borderStyle solid
            , borderColor (hex "#f5f5f5")
            , borderRadius (px 6)
            , padding (px 15)
            , backgroundColor (hex "#fdfdfd")
            , marginBottom (px 15)
            ]


inputRowStyles : List Style
inputRowStyles =
    [ displayFlex
    , marginBottom (px 15)
    , alignItems center
    ]


listInputStyles : List Style
listInputStyles =
    [ flex (int 1), marginRight (px 8) ]


secondaryInputStyles : List Style
secondaryInputStyles =
    [ width (em 3), marginRight (px 8) ]


inputLabelStyles : List Style
inputLabelStyles =
    [ marginRight (px 5) ]


codeLineStyles : List Style
codeLineStyles =
    [ backgroundColor (hex "#f6f8fa")
    , padding2 (em 0.2) (em 0.4)
    ]


codeBlockStyles : List Style
codeBlockStyles =
    [ backgroundColor (hex "#f6f8fa")
    , padding2 (em 0.2) (em 0.4)
    , padding (px 10)
    , overflow scroll
    ]


linkStyles : List Style
linkStyles =
    [ textDecoration none
    , color (hex "#1293D8")
    , hover
        [ textDecoration underline ]
    , fontFamilies [ "Source Code Pro", "Consolas", "Liberation Mono", "Menlo", "Courier", "monospace" ]
    ]


buttonStyles : List Style
buttonStyles =
    [ backgroundColor inherit
    , color (hex "#596277")
    , border (px 0)
    , padding (px 0)
    , fontFamily inherit
    , fontSize inherit
    , cursor pointer
    , outline inherit
    , display inlineFlex
    , alignItems center
    , hover
        [ color (hex "#8CD636")
        , textDecoration underline
        ]
    ]


hamburgerButtonStyles : Bool -> List Style
hamburgerButtonStyles mobileMenuOpen =
    [ border (px 1)
    , borderColor (hex "#ffffff60")
    , borderStyle solid
    , borderRadius (px 6)
    , backgroundColor inherit
    , padding (px 4)
    , marginLeft auto
    , fontFamily inherit
    , fontSize inherit
    , cursor pointer
    , outline inherit
    , display inlineFlex
    , alignItems center
    , transition
        [ Css.Transitions.backgroundColor 200
        , Css.Transitions.borderColor 200
        , Css.Transitions.boxShadow 200
        ]
    ]
        ++ (if mobileMenuOpen then
                [ backgroundColor (hex "#EEA400")
                , borderColor (hex "#EEA400")
                , boxShadow5 (px 0) (px 0) (px 5) (px 2) (hex "#EEA400")
                ]

            else
                []
           )


sideBarItemListStyles : List Style
sideBarItemListStyles =
    [ listStyleType none
    , margin (px 0)
    , padding (px 0)
    , color (hex "#1293D8")
    , fontSize (px 16)
    , lineHeight (em 1.5)
    ]


disabledBackground : Float -> msg -> Bool -> Html msg
disabledBackground opacityValue hideMsg isVisible =
    div
        [ onClick hideMsg
        , css <|
            [ opacity (num 0)
            , visibility hidden
            , cursor default
            , position fixed
            , zIndex (int 1)
            , top (px 0)
            , left (px 0)
            , width (pct 100)
            , height (pct 100)
            , backgroundColor (hex "#596277")
            , transition
                [ Css.Transitions.opacity 200
                , Css.Transitions.visibility 200
                ]
            ]
                ++ (if isVisible then
                        [ opacity (num opacityValue)
                        , visibility visible
                        ]

                    else
                        []
                   )
        ]
    <|
        if isVisible then
            [ global
                [ selector "body"
                    [ overflow hidden
                    ]
                ]
            ]

        else
            []



-- SYNTAX HIGHLIGHT


syntaxHighlightRequiredCssNode : Html.Styled.Html msg
syntaxHighlightRequiredCssNode =
    node "style" [] [ text syntaxHighlightRequiredRawCss ]



{-
   You need to remove the call to SyntaxHighlight.useTheme in order for this css to apply.
-}


syntaxHighlightThemeCssNode : Html.Styled.Html msg
syntaxHighlightThemeCssNode =
    node "style" [] [ text syntaxHighlightThemeRawCss ]


syntaxHighlightRequiredRawCss : String
syntaxHighlightRequiredRawCss =
    """
/* Elm Syntax Highlight CSS */
pre.elmsh {
  padding: 10px;
  margin: 0;
  text-align: left;
  overflow: auto;
  border-radius: 6px;
  background-color: #ffffff;
}

code.elmsh {
  padding: 0;
}

.elmsh-line:before {
  content: attr(data-elmsh-lc);
  display: inline-block;
  text-align: right;
  width: 40px;
  padding: 0 20px 0 0;
  opacity: 0.3;
}

"""


syntaxHighlightThemeRawCss : String
syntaxHighlightThemeRawCss =
    """
.elmsh {
    color: #24292e;
    background: #ffffff;
}
.elmsh-hl {
    background: #fffbdd;
}
.elmsh-add {
    background: #eaffea;
}
.elmsh-del {
    background: #ffecec;
}
.elmsh-comm {
    color: #969896;
}
.elmsh1 {
    color: #005cc5;
}
.elmsh2 {
    color: #df5000;
}
.elmsh3 {
    color: #d73a49;
}
.elmsh4 {
    color: #0086b3;
}
.elmsh5 {
    color: #63a35c;
}
.elmsh6 {
    color: #005cc5;
}
.elmsh7 {
    color: #795da3;
}
.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {
    font-style: italic;
    color: #66d9ef;
}
.elmsh-js-ce {
    font-style: italic;
    color: #a6e22e;
}
.elmsh-css-ar-i {
    font-weight: bold;
    color: #f92672;
}
"""



-- GENERIC STYLES


genericStylesNode : Html.Styled.Html msg
genericStylesNode =
    node "style" [] [ text genericRawCss ]


genericRawCss : String
genericRawCss =
    """
body {
    margin: 0;
    font-family: 'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif;
    color: #000E16;
}

input {
    padding: 4px;
    border: 1px solid #eeeeee;
    border-radius: 6px;
}

input:focus {
    outline-color: #a6e22e;
    outline-style: solid;
    outline-width: 1px;
    
}

table {
    border-collapse: collapse;

}

td {
    border: 1px solid #f5f5f5;
    padding: 0.2em;
    min-width: 5em;
    text-align: center;
}


"""
