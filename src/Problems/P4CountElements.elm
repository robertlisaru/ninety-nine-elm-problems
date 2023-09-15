module Problems.P4CountElements exposing (countElements)


countElements : List a -> Int
countElements list =
    list |> List.map (\_ -> 1) |> List.sum



{- countElements2 : List a -> Int
   countElements2 list =
       list |> List.foldl ((\_ -> 1) >> (+)) 0
-}
