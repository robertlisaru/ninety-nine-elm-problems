module Utils exposing (boolToString, displayIf, emptyListMeansNothing, intToTwoDigitString, listOfListsToString, listToString, maybeToString, nestedListToString, rleCodeToString, scrollToElementId, tupleToString)

import Browser.Dom as Dom
import Html.Styled exposing (Html, text)
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P7FlattenNestedList exposing (NestedList(..))
import Task


listToString : (a -> String) -> String -> List a -> String
listToString convert separator list =
    "[" ++ (list |> List.map convert |> String.join separator) ++ "]"


maybeToString : (a -> String) -> Maybe a -> String
maybeToString convert maybe =
    maybe |> Maybe.map convert |> Maybe.withDefault "Nothing"


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


tupleToString : (a -> String) -> (b -> String) -> ( a, b ) -> String
tupleToString aToString bToString ( a, b ) =
    "(" ++ aToString a ++ ", " ++ bToString b ++ ")"


listOfListsToString : List (List Int) -> String
listOfListsToString listOfLists =
    listOfLists |> listToString (listToString String.fromInt ", ") ", "


rleCodeToString : RleCode Int -> String
rleCodeToString rleCode =
    case rleCode of
        Run count element ->
            "Run " ++ String.fromInt count ++ " " ++ String.fromInt element

        Single element ->
            "Single " ++ String.fromInt element


intToTwoDigitString : Int -> String
intToTwoDigitString number =
    let
        shortened =
            number |> remainderBy 100 |> abs
    in
    if shortened < 10 then
        "0" ++ String.fromInt shortened

    else
        String.fromInt shortened


scrollToElementId : (Result Dom.Error () -> msg) -> Float -> String -> Cmd msg
scrollToElementId message scrollOffsetY htmlElementId =
    Dom.getElement htmlElementId
        |> Task.andThen
            (\item ->
                Dom.setViewport item.element.x (item.element.y + scrollOffsetY)
            )
        |> Task.attempt message


emptyListMeansNothing : List a -> Maybe (List a)
emptyListMeansNothing list =
    if list |> List.isEmpty then
        Nothing

    else
        Just list
