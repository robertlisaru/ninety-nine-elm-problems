module IsPalindrome exposing (..)

import Expect
import Problems.IsPalindrome exposing (isPalindrome)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ isPalindrome [ 1, 3, 5, 8, 5, 3, 1 ] == True
                        , isPalindrome [ 2, 1 ] == False
                        , isPalindrome [ 1 ] == True
                        , isPalindrome [] == True
                        , isPalindrome [ "aa", "bb", "aa" ] == True
                        , isPalindrome [ "aab", "b", "aa" ] == False
                        ]
