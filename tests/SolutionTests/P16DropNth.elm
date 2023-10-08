module SolutionTests.P16DropNth exposing (suite)

import Expect
import Solutions.P16DropNth exposing (dropNth)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ dropNth 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 5, 2 ]
                        , dropNth 3 (List.range 1 20) == [ 1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20 ]
                        , dropNth 6 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropNth 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropNth -1 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , dropNth 1 (List.range 1 5) == []
                        , dropNth 2 [ "1", "2", "3", "4", "5", "6" ] == [ "1", "3", "5" ]
                        ]
