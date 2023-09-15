module Problems.IsPalindrome exposing (isPalindrome)

import List


isPalindrome : List a -> Bool
isPalindrome list =
    let
        reversed =
            list |> List.reverse
    in
    list == reversed
