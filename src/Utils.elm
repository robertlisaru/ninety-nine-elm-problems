module Utils exposing (displayIf, listToString, maybeToString)

import Html.Styled exposing (Html, text)


listToString : (a -> String) -> String -> List a -> String
listToString convert separator list =
    "[" ++ (list |> List.map convert |> String.join separator) ++ "]"


maybeToString : (a -> String) -> Maybe a -> String
maybeToString convert maybe =
    maybe |> Maybe.map (\a -> "Just " ++ convert a) |> Maybe.withDefault "Nothing"


displayIf : Bool -> Html msg -> Html msg
displayIf shouldDisplay element =
    if shouldDisplay then
        element

    else
        text ""
