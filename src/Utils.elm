module Utils exposing (boolToString, displayIf, listOfListsToString, listToString, maybeToString, nestedListToString, tupleToString)

import Html.Styled exposing (Html, text)
import Solutions.P7FlattenNestedList exposing (NestedList(..))


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


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


nestedListToString : Solutions.P7FlattenNestedList.NestedList Int -> String
nestedListToString nl =
    case nl of
        Elem a ->
            String.fromInt a

        SubList list ->
            list |> listToString nestedListToString ", "


tupleToString : ( Int, Int ) -> String
tupleToString ( a, b ) =
    "(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"


listOfListsToString : List (List Int) -> String
listOfListsToString listOfLists =
    listOfLists |> listToString (listToString String.fromInt ", ") ", "
