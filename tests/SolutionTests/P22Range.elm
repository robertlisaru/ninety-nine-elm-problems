module SolutionTests.P22Range exposing (suite)

import Expect
import Solutions.P22Range exposing (range)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ range 1 5 == [ 1, 2, 3, 4, 5 ]
                        , range 0 5 == [ 0, 1, 2, 3, 4, 5 ]
                        , range -1 5 == [ -1, 0, 1, 2, 3, 4, 5 ]
                        , range 5 -1 == [ 5, 4, 3, 2, 1, 0, -1 ]
                        , range 5 5 == [ 5 ]
                        , List.length (range 1 999) == 999
                        ]
