module PageElements exposing (appIntroView, navView, sideBarView)

import Css exposing (..)
import Html.Styled exposing (Html, a, div, h1, h2, input, li, nav, span, text, ul)
import Html.Styled.Attributes exposing (css, href, placeholder, value)
import Html.Styled.Events exposing (onInput)
import ProblemHeaders exposing (problemHeaders)
import Styles
    exposing
        ( linkStyles
        , navStyles
        , searchBarStyles
        , sideBarItemListStyles
        , sideBarStyles
        )
import SvgItems


navView : Html msg
navView =
    let
        navItem url label =
            a
                [ css [ textDecoration none, color (hex "#ffffff"), hover [ textDecoration underline ] ]
                , href url
                ]
                [ text label ]
    in
    nav [ css navStyles ]
        [ logoView
        , h1 [ css [ fontSize (px 24), fontWeight normal, color (hex "#ffffff") ] ]
            [ navItem "https://github.com/robertlisaru" "robertlisaru"
            , span [ css [ margin2 (px 0) (px 10) ] ] [ text "/" ]
            , navItem "https://github.com/robertlisaru/ninety-nine-elm-problems" "ninety-nine-elm-problems"
            ]
        ]


logoView : Html msg
logoView =
    a
        [ css
            [ textDecoration none
            , marginRight (px 32)
            , displayFlex
            , alignItems center
            ]
        ]
        [ SvgItems.elmLogo
        , div [ css [ paddingLeft (px 8), color (hex "#ffffff") ] ]
            [ div [ css [ lineHeight (px 24), fontSize (px 30) ] ] [ text "elm" ]
            , div [ css [ fontSize (px 12) ] ] [ text "99 problems" ]
            ]
        ]


appIntroView : Html msg
appIntroView =
    div []
        [ h1 [ css [ fontSize (em 3), marginBottom (px 0), fontWeight normal ] ] [ text "99 Elm problems" ]
        , h2 [ css [ fontSize (px 16), marginTop (px 0), marginBottom (px 50), lineHeight (em 1.5), fontWeight normal ] ]
            [ text "Sharpen your functional programming skills." ]
        ]


sideBarView : String -> (String -> msg) -> Html msg
sideBarView searchKeyWord searchMsg =
    let
        filteredProblems =
            problemHeaders
                |> List.filter
                    (\problem ->
                        (String.fromInt problem.number ++ ". " ++ problem.title)
                            |> String.toLower
                            |> String.contains (searchKeyWord |> String.toLower)
                    )

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
        , ul [ css sideBarItemListStyles ]
            [ linkItem "https://github.com/evancz" "Elm was created by Evan Czaplicki"
            , linkItem "https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/" "The 99 problems are adapted to Elm in a gitbook by johncrane"
            , linkItem "https://package.elm-lang.org/" "This page layout is inspired by the official Elm Packages website"
            , linkItem "https://elm-lang.org/" "Visit the official Elm Website"
            ]
        , h2 [ css [ marginBottom (px 0), fontWeight normal ] ] [ text "Problems" ]
        , input [ placeholder "Search", Html.Styled.Attributes.type_ "search", css searchBarStyles, value searchKeyWord, onInput searchMsg ] []
        , ul [ css sideBarItemListStyles ]
            (filteredProblems
                |> List.map
                    (\problem ->
                        li []
                            [ a
                                [ href ("#" ++ (problem.number |> String.fromInt))
                                , css linkStyles
                                ]
                                [ text <| String.fromInt problem.number ++ ". " ++ problem.title ]
                            ]
                    )
            )
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