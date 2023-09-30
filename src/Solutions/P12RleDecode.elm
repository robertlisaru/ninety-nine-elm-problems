module Solutions.P12RleDecode exposing (rleDecode)

import List
import Solutions.P11RleEncode exposing (RleCode(..))


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
