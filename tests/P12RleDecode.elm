module P12RleDecode exposing (..)

import Expect
import List
import Problems.P12RleDecode exposing (RleCode(..), rleDecode)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ rleDecode [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
                            == [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                        , rleDecode [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
                            == [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                        , rleDecode [ Run 4 "1", Single "b", Run 2 "5", Single "2", Single "a" ]
                            == [ "1", "1", "1", "1", "b", "5", "5", "2", "a" ]
                        ]
