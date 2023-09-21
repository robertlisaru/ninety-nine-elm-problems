module SolutionTests.P18Sublist exposing (suite)

import Expect
import Solutions.P18Sublist exposing (sublist)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ sublist 3 7 (List.range 1 10) == List.range 3 7
                        , sublist 2 100 [ 'a', 'b', 'c' ] == [ 'b', 'c' ]
                        , sublist -1 2 (List.range 1 100) == [ 1, 2 ]
                        , sublist -3 -2 [ -3, -2, -1, 0, 1, 2, 3 ] == []
                        , sublist 5 3 [ "indices", " are", "inverted" ] == []
                        , sublist 0 1 (List.range 1 10) == [ 1 ]
                        , sublist -7 -3 (List.range 1 10) == []
                        ]
