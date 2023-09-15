module DropNth exposing (..)

import Expect
import Problems.DropNth exposing (dropNth)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ dropNth [ 1, 2, 5, 5, 2, 1 ] 2 == [ 1, 5, 2 ]
                        , dropNth (List.range 1 20) 3 == [ 1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20 ]
                        , dropNth (List.range 1 5) 6 == [ 1, 2, 3, 4, 5 ]
                        , dropNth (List.range 1 5) 0 == [ 1, 2, 3, 4, 5 ]
                        , dropNth (List.range 1 5) -1 == [ 1, 2, 3, 4, 5 ]
                        , dropNth (List.range 1 5) 1 == []
                        , dropNth [ "1", "2", "3", "4", "5", "6" ] 2 == [ "1", "3", "5" ]
                        ]
