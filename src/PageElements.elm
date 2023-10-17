module PageElements exposing (appIntroView, navView, sideBarView)

import Css exposing (..)
import Html.Styled exposing (Html, a, button, div, h1, h2, input, li, nav, span, text, ul)
import Html.Styled.Attributes exposing (css, href, placeholder, value)
import Html.Styled.Events exposing (onInput)
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
import Utils exposing (DeviceType(..))


navView : DeviceType -> Html msg
navView deviceType =
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
                        [ fontSize (px 18)
                        , fontWeight normal
                        , color (hex "#ffffff")
                        , displayFlex
                        , flexWrap wrap
                        , lineHeight (em 1)
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
        , (Utils.displayIf <| Utils.isMobile <| deviceType)
            (button [ css hamburgerButtonStyles ]
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
        [ SvgItems.elmLogo
        , div [ css [ paddingLeft (px 8), color (hex "#ffffff"), width (px 64) ] ]
            [ div [ css [ lineHeight (px 24), fontSize (px 30) ] ] [ text "elm" ]
            , div [ css [ fontSize (px 12) ] ] [ text "99 problems" ]
            ]
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


sideBarView : String -> (String -> msg) -> Html msg
sideBarView searchKeyWord searchMsg =
    let
        linkItem url label =
            li [] [ a [ href url, css linkStyles ] [ text label ] ]
    in
    div [ css sideBarStyles ]
        [ ul [ css sideBarItemListStyles ]
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
            [ linkItem "https://github.com/evancz" "Elm was created by Evan Czaplicki"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "The 99 problems are adapted to Elm in a gitbook by johncrane"
            , linkItem "https://package.elm-lang.org/" "This page layout is inspired by the official Elm Packages website"
            , linkItem "https://elm-lang.org/" "Visit the official Elm Website"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Problems" ]
        , input [ placeholder "Search", Html.Styled.Attributes.type_ "search", css searchBarStyles, value searchKeyWord, onInput searchMsg ] []
        , ul [ css sideBarItemListStyles ]
            (categories |> List.map (viewCategory searchKeyWord))
        , div [ css [ position sticky, top (px 20), marginTop (px 20) ] ]
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


viewProblemHeader : ProblemHeader -> Html msg
viewProblemHeader problem =
    li []
        [ a
            [ href ("#" ++ (problem.number |> String.fromInt))
            , css linkStyles
            ]
            [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
        ]


viewCategory : String -> Category -> Html msg
viewCategory keyword category =
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
                    (filteredProblems |> List.map viewProblemHeader)
                ]
