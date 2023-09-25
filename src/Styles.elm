module Styles exposing (codeStyles, headerStyles, leftContentStyles, pageContainerStyles, problemInteractiveAreaStyles, problemListStyles, problemStyles, problemTitleStyles, searchBarStyles, sideBarStyles, syntaxHighlightRequiredCssNode, syntaxHighlightThemeCssNode)

import Css exposing (..)
import Html.Styled exposing (node, text)


headerStyles : List Style
headerStyles =
    [ height (px 64)
    , backgroundColor (hex "#5FABDC")
    , color (hex "#ffffff")
    , width (calc (pct 100) minus (px 40))
    , paddingLeft (px 20)
    , paddingRight (px 20)
    , overflowX hidden
    ]


pageContainerStyles : List Style
pageContainerStyles =
    [ maxWidth (px 920)
    , marginLeft auto
    , marginRight auto
    ]


leftContentStyles : List Style
leftContentStyles =
    [ maxWidth (px 600)
    , width (pct 75)
    , display inlineBlock
    , verticalAlign top
    ]


sideBarStyles : List Style
sideBarStyles =
    [ maxWidth (px 200)
    , width (pct 25)
    , display inlineBlock
    , verticalAlign top
    , padding (px 20)
    , margin4 (px 20) (px 20) (px 20) (px 40)
    , borderLeft3 (px 1) solid (hex "#eeeeee")
    , minHeight (px 500)
    ]


searchBarStyles : List Style
searchBarStyles =
    [ fontSize (em 1)
    , padding (px 4)
    , margin2 (px 10) (px 0)
    , border3 (px 1) solid (hex "#eeeeee")
    , borderRadius (px 6)
    ]


problemListStyles : List Style
problemListStyles =
    [ listStyleType none
    , padding (px 0)
    , marginTop (px 25)
    ]


problemTitleStyles : List Style
problemTitleStyles =
    [ color (hex "#1293D8")
    , textDecoration none
    , fontSize (em 1.5)
    , fontWeight normal
    ]


problemStyles : List Style
problemStyles =
    [ borderWidth (px 1)
    , borderStyle solid
    , borderColor (rgb 150 150 150)
    , borderRadius (px 6)
    , boxShadow4 (px 3) (px 3) (px 3) (rgba 0 0 0 0.5)
    , marginBottom (px 50)
    , padding (px 25)
    , paddingTop (px 0)
    , backgroundColor (rgb 200 255 255)
    ]


problemInteractiveAreaStyles : List Style
problemInteractiveAreaStyles =
    [ borderWidth (px 1)
    , borderStyle solid
    , borderColor (rgb 150 150 150)
    , borderRadius (px 6)
    , padding (px 10)
    , paddingTop (px 0)
    ]


codeStyles : List Style
codeStyles =
    [ Css.backgroundColor (Css.hex "#f6f8fa"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ]


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
