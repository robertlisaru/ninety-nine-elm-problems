module Problems.RepeatElements exposing (repeatElements)

{- Using foldr

   repeat : Int -> a -> List a -> List a
   repeat count element list =
       List.repeat count element ++ list


   repeatElements : Int -> List a -> List a
   repeatElements count list =
       list |> List.foldr (repeat count) []
-}


repeatElements : Int -> List a -> List a
repeatElements count list =
    list |> List.concatMap (\a -> List.repeat count a)
