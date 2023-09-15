module RleEncode exposing (..)

import Expect
import Problems.RleEncode exposing (RleCode(..), rleEncode)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                            == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
                        , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
                        , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
                        , rleEncode [ 1 ] == [ Single 1 ]
                        , rleEncode [] == []
                        , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
                        , rleEncode [ "aab", "b", "b", "aa" ]
                            == [ Single "aab", Run 2 "b", Single "aa" ]
                        ]
