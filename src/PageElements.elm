module PageElements exposing (appIntroView, navView, sideBarView)

import Css exposing (..)
import DeviceType exposing (DeviceType(..), isMobile)
import Html.Styled exposing (Html, a, button, div, h1, h2, input, li, nav, span, text, ul)
import Html.Styled.Attributes exposing (css, href, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import ProblemHeaders exposing (Category, ProblemHeader, categories)
import Styles
    exposing
        ( hamburgerButtonStyles
        , linkStyles
        , navStyles
        , searchBarStyles
        , sideBarItemListStyles
        , sideBarStyles
        , subHeadingStyles
        )
import SvgItems
import Utils


navView : DeviceType -> Bool -> msg -> Html msg
navView deviceType mobileMenuOpen mobileMenuMsg =
    let
        navItem url label =
            a
                [ css [ textDecoration none, color (hex "#ffffff"), hover [ textDecoration underline ] ]
                , href url
                ]
                [ text label ]
    in
    nav [ css navStyles ]
        [ logoView deviceType
        , h1
            [ css <|
                case deviceType of
                    Mobile ->
                        [ fontSize (px 16)
                        , fontWeight normal
                        , color (hex "#ffffff")
                        , displayFlex
                        , flexWrap wrap
                        , lineHeight (em 1)
                        , marginTop (px 0)
                        , marginBottom (px 0)
                        , marginRight (px 8)
                        ]

                    Desktop ->
                        [ fontSize (px 24)
                        , fontWeight normal
                        , color (hex "#ffffff")
                        ]
            ]
            [ navItem "https://github.com/robertlisaru" "robertlisaru"
            , span
                [ css <|
                    case deviceType of
                        Mobile ->
                            [ margin2 (px 0) (px 4) ]

                        Desktop ->
                            [ margin2 (px 0) (px 10) ]
                ]
                [ text "/" ]
            , navItem "https://github.com/robertlisaru/ninety-nine-elm-problems" "ninety-nine-elm-problems"
            ]
        , (Utils.displayIf <| isMobile <| deviceType)
            (button [ onClick mobileMenuMsg, css <| hamburgerButtonStyles <| mobileMenuOpen ]
                [ SvgItems.hamburger ]
            )
        ]


logoView : DeviceType -> Html msg
logoView deviceType =
    a
        [ css
            [ textDecoration none
            , marginRight <|
                case deviceType of
                    Mobile ->
                        px 8

                    Desktop ->
                        px 32
            , displayFlex
            , alignItems center
            ]
        ]
        [ SvgItems.elmLogo <|
            case deviceType of
                Mobile ->
                    28

                Desktop ->
                    32
        , div
            [ css <|
                case deviceType of
                    Mobile ->
                        [ paddingLeft (px 4), color (hex "#ffffff"), width (px 64) ]

                    Desktop ->
                        [ paddingLeft (px 8), color (hex "#ffffff"), width (px 64) ]
            ]
            [ div
                [ css <|
                    case deviceType of
                        Mobile ->
                            [ lineHeight (px 18)
                            , fontSize (px 24)
                            ]

                        Desktop ->
                            [ lineHeight (px 24)
                            , fontSize (px 30)
                            ]
                ]
                [ text "elm" ]
            , div [ css [ fontSize (px 12) ] ] [ text "99 problems" ]
            ]
        ]


darkModeButton : msg -> Bool -> Html msg
darkModeButton toggleDarkMode darkMode =
    div
        [ css
            [ display inlineFlex
            , alignItems center
            , justifyContent spaceBetween
            , marginBottom (px 20)
            ]
        ]
        [ button
            [ onClick toggleDarkMode
            , css <|
                [ border (px 0)
                , padding (px 0)
                , fontFamily inherit
                , fontSize inherit
                , cursor pointer
                , outline inherit
                , display inlineFlex
                , alignItems center
                , height (px 24)
                , width (px 48)
                , borderRadius (px 20)
                , borderStyle solid
                , borderWidth (px 1)
                , padding (px 2)
                , borderColor (hex "#596277")
                , marginRight (px 8)
                ]
                    ++ (if darkMode then
                            [ justifyContent end
                            , backgroundColor (hex "#59627780")
                            ]

                        else
                            [ justifyContent start
                            , backgroundColor (hex "#59627710")
                            ]
                       )
            ]
            [ div
                [ css
                    [ backgroundColor (hex "#fff")
                    , width (px 18)
                    , height (px 18)
                    , border (px 0)
                    , borderRadius (pct 50)
                    , borderStyle solid
                    , borderWidth (px 1)
                    , borderColor (hex "#596277a0")
                    , padding (px 0)
                    , fontFamily inherit
                    , fontSize inherit
                    , cursor pointer
                    , outline inherit
                    , display inlineFlex
                    , alignItems center
                    ]
                ]
                []
            ]
        , if darkMode then
            SvgItems.moon

          else
            SvgItems.sun
        ]


appIntroView : DeviceType -> Html msg
appIntroView deviceType =
    div []
        [ h1
            [ css <| Styles.mainHeadingStyles <| deviceType
            ]
            [ text "99 Elm problems" ]
        , h2
            [ css <| subHeadingStyles <| deviceType ]
            [ text "Sharpen your functional programming skills." ]
        ]


sideBarView :
    msg
    -> Bool
    -> (String -> msg)
    -> DeviceType
    -> Bool
    -> String
    -> (String -> msg)
    -> Html msg
sideBarView toggleDarkMode darkMode scrollToElementId deviceType isOpen searchKeyWord searchMsg =
    let
        linkItem url label =
            li [] [ a [ href url, css linkStyles ] [ text label ] ]
    in
    nav [ css <| sideBarStyles isOpen <| deviceType ]
        [ Utils.displayIf False <| darkModeButton toggleDarkMode darkMode
        , ul [ css sideBarItemListStyles ]
            [ linkItem "" "README"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "About"
            , linkItem "https://github.com/robertlisaru/ninety-nine-elm-problems" "Source"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Credits" ]
        , ul
            [ css
                (sideBarItemListStyles
                    ++ [ property "list-style-type" "\"\\1F680\""
                       , paddingLeft (px 20)
                       ]
                )
            ]
            [ linkItem "https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/" "The original collection was named Ninety-Nine Prolog Problems by Werner Hett"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "The problems are adapted to Elm in a gitbook by John Crane"
            , linkItem "https://github.com/evancz" "Elm was created by Evan Czaplicki"
            , linkItem "https://elm-lang.org/" "Visit the official Elm Website"
            , linkItem "https://package.elm-lang.org/" "This page layout is inspired by the official Elm Packages website"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Problems" ]
        , input [ placeholder "Search", Html.Styled.Attributes.type_ "search", css searchBarStyles, value searchKeyWord, onInput searchMsg ] []
        , ul [ css sideBarItemListStyles ]
            (categories |> List.map (viewCategory scrollToElementId deviceType searchKeyWord))
        , Utils.displayIf (deviceType == Desktop) <|
            div [ css [ position sticky, top (px 20), marginTop (px 20) ] ]
                [ a
                    [ css
                        (linkStyles
                            ++ [ displayFlex
                               , alignItems center
                               ]
                        )
                    , href "#"
                    ]
                    [ SvgItems.top
                    , div [ css [ marginRight (em 0.6) ] ] []
                    , text "Back to top"
                    ]
                ]
        ]


viewProblemHeader : (String -> msg) -> DeviceType -> ProblemHeader -> Html msg
viewProblemHeader scrollToElementId deviceType problem =
    li []
        [ a
            [ case deviceType of
                Mobile ->
                    onClick (problem.number |> String.fromInt |> scrollToElementId)

                Desktop ->
                    href ("#" ++ (problem.number |> String.fromInt))
            , css linkStyles
            ]
            [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
        ]


viewCategory : (String -> msg) -> DeviceType -> String -> Category -> Html msg
viewCategory scrollToElementId deviceType keyword category =
    let
        filteredProblems =
            category.problems
                |> List.filter
                    (\problem ->
                        (String.fromInt problem.number ++ ". " ++ problem.title)
                            |> String.toLower
                            |> String.contains (keyword |> String.toLower)
                    )
    in
    case filteredProblems of
        [] ->
            text ""

        _ ->
            div []
                [ span [ css (linkStyles ++ [ color (hex "#EEA400") ]) ] [ text category.title ]
                , ul [ css (sideBarItemListStyles ++ [ paddingLeft (px 20) ]) ]
                    (filteredProblems |> List.map (viewProblemHeader scrollToElementId deviceType))
                ]
