module SolutionTests.P6IsPalindrome exposing (suite)

import Expect
import Solutions.P6IsPalindrome exposing (isPalindrome)
import Test exposing (Test, test)


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
