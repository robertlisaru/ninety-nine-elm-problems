module Utils exposing (listToString)


listToString : (a -> String) -> String -> List a -> String
listToString convert separator list =
    "["
        ++ (list
                |> List.map convert
                |> List.intersperse separator
                |> List.foldl (++) "]"
           )
