module P15RepeatElements exposing (..)

import Expect
import Problems.P15RepeatElements exposing (repeatElements)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ repeatElements 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 1, 2, 2, 5, 5, 5, 5, 2, 2, 1, 1 ]
                        , repeatElements 4 [ 1, 2 ] == [ 1, 1, 1, 1, 2, 2, 2, 2 ]
                        , repeatElements 4 [] == []
                        , repeatElements 0 [ 1, 2 ] == []
                        , repeatElements -1 [ 1, 2 ] == []
                        , repeatElements 40 [ 1 ] == [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
                        , repeatElements 4 [ "1", "2" ] == [ "1", "1", "1", "1", "2", "2", "2", "2" ]
                        ]
