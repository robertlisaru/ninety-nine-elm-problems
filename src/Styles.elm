module Styles exposing (codeStyles, problemListStyles, problemStyles)

import Css exposing (..)


problemStyles : List Style
problemStyles =
    [ borderWidth (px 1)
    , borderStyle solid
    , borderColor (rgb 150 150 150)
    , borderRadius (px 5)
    , boxShadow4 (px 3) (px 3) (px 3) (rgba 0 0 0 0.5)
    , marginBottom (px 25)
    , maxWidth (px 775)
    , minWidth (px 300)
    , width (pct 80)
    , padding (px 25)
    , paddingTop (px 0)
    , backgroundColor (rgb 200 255 255)
    ]


problemListStyles : List Style
problemListStyles =
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


codeStyles : List Style
codeStyles =
    [ Css.backgroundColor (Css.hex "#f6f8fa"), Css.padding2 (Css.em 0.2) (Css.em 0.4) ]
