module SvgItems exposing (elmLogo)

import Html.Styled exposing (Html, fromUnstyled)
import Svg exposing (svg)
import Svg.Attributes as SvgAttr


elmLogo : Html msg
elmLogo =
    svg
        [ SvgAttr.height "32"
        , SvgAttr.viewBox "0 0 600 600"
        ]
        [ Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "0,20 280,300 0,580"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "20,600 300,320 580,600"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "320,0 600,0 600,280"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "20,0 280,0 402,122 142,122"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "170,150 430,150 300,280"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "320,300 450,170 580,300 450,430"
            ]
            []
        , Svg.polygon
            [ SvgAttr.fill "currentColor"
            , SvgAttr.points "470,450 600,320 600,580"
            ]
            []
        ]
        |> fromUnstyled
