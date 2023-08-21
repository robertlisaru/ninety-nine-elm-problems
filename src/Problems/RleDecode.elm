module Problems.RleDecode exposing (test)

import List



{- Decompress the run-length encoded list generated in Problem 11. -}


type RleCode a
    = Run Int a
    | Single a


expand : RleCode a -> List a
expand rleCode =
    case rleCode of
        Run length a ->
            List.repeat length a

        Single a ->
            [ a ]


rleDecode : List (RleCode a) -> List a
rleDecode list =
    list |> List.concatMap expand


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ rleDecode [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
                == [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
            , rleDecode [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
                == [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
            , rleDecode [ Run 4 "1", Single "b", Run 2 "5", Single "2", Single "a" ]
                == [ "1", "1", "1", "1", "b", "5", "5", "2", "a" ]
            ]
