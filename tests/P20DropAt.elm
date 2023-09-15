module P20DropAt exposing (..)

import Expect
import Problems.P20DropAt exposing (dropAt)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ dropAt 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 5, 5, 2, 1 ]
                        , dropAt 3 (List.range 1 14) == [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
                        , dropAt 6 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropAt 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropAt -1 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropAt 1 (List.range 1 5) == [ 2, 3, 4, 5 ]
                        , dropAt 2 [ "1", "2", "3", "4", "5" ] == [ "1", "3", "4", "5" ]
                        ]
