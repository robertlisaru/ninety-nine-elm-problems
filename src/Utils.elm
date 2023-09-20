module Utils exposing (listToString, maybeToString)


listToString : (a -> String) -> String -> List a -> String
listToString convert separator list =
    "["
        ++ (list
                |> List.map convert
                |> List.intersperse separator
                |> List.foldr (++) "]"
           )


maybeToString : (a -> String) -> Maybe a -> String
maybeToString convert maybe =
    maybe |> Maybe.map (\a -> "Just " ++ convert a) |> Maybe.withDefault "Nothing"
