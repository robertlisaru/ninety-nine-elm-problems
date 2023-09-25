module SvgItems exposing (elmLogo)

import Css exposing (fill, hex, hover)
import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as SvgAttr


elmLogo : Html msg
elmLogo =
    svg
        [ SvgAttr.height "32"
        , SvgAttr.viewBox "0 0 600 600"
        ]
        [ Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "0,20 280,300 0,580"
            , SvgAttr.css [ hover [ fill (hex "#596277") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "20,600 300,320 580,600"
            , SvgAttr.css [ hover [ fill (hex "#5FB4CB") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "320,0 600,0 600,280"
            , SvgAttr.css [ hover [ fill (hex "#5FB4CB") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "20,0 280,0 402,122 142,122"
            , SvgAttr.css [ hover [ fill (hex "#8CD636") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "170,150 430,150 300,280"
            , SvgAttr.css [ hover [ fill (hex "#EEA400") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "320,300 450,170 580,300 450,430"
            , SvgAttr.css [ hover [ fill (hex "#8CD636") ] ]
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "470,450 600,320 600,580"
            , SvgAttr.css [ hover [ fill (hex "#EEA400") ] ]
            ]
            []
        ]
