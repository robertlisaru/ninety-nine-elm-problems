module SolutionTests.P4CountElements exposing (suite)

import Expect
import Solutions.P4CountElements exposing (countElements)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ countElements (List.range 1 4000) == 4000
                        , countElements [ 1 ] == 1
                        , countElements [] == 0
                        , countElements [ 'a', 'b', 'c' ] == 3
                        ]
