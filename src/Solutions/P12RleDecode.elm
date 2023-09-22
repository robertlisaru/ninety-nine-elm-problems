module Solutions.P12RleDecode exposing (RleCode(..), rleDecode)

import List


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
