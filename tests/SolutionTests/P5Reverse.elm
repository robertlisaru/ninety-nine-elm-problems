module SolutionTests.P5Reverse exposing (suite)

import Expect
import Solutions.P5Reverse exposing (myReverse)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ myReverse [ 1, 2, 3, 4 ] == [ 4, 3, 2, 1 ]
                        , myReverse [ 2, 1 ] == [ 1, 2 ]
                        , myReverse [ 1 ] == [ 1 ]
                        , myReverse [] == []
                        , myReverse [ 'a', 'b', 'c' ] == [ 'c', 'b', 'a' ]
                        ]
