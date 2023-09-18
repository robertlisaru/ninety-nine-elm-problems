module Styles exposing (problemListStyles, problemStyles)

import Css exposing (..)


problemStyles : List Style
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
