module P9Pack exposing (..)

import Expect
import Problems.P9Pack exposing (pack)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ pack [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                        , pack [ 2, 1, 1, 1 ] == [ [ 2 ], [ 1, 1, 1 ] ]
                        , pack [ 2, 2, 2, 1, 1, 1 ] == [ [ 2, 2, 2 ], [ 1, 1, 1 ] ]
                        , pack [ 1 ] == [ [ 1 ] ]
                        , pack [] == []
                        , pack [ "aa", "aa", "aa" ] == [ [ "aa", "aa", "aa" ] ]
                        , pack [ "aab", "b", "b", "aa" ] == [ [ "aab" ], [ "b", "b" ], [ "aa" ] ]
                        ]
