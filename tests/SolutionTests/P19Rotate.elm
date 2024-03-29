module SolutionTests.P19Rotate exposing (suite)

import Expect
import Solutions.P19Rotate exposing (rotate)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ rotate 3 [ 1, 2, 5, 5, 2, 1 ] == [ 5, 2, 1, 1, 2, 5 ]
                        , rotate 13 (List.range 1 10) == [ 4, 5, 6, 7, 8, 9, 10, 1, 2, 3 ]
                        , rotate 1 (List.range 1 5) == [ 2, 3, 4, 5, 1 ]
                        , rotate 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
                        , rotate -1 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
                        , rotate -6 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
                        , rotate 3 (List.range 1 5) == [ 4, 5, 1, 2, 3 ]
                        , rotate 1 [ "1", "2", "3", "4" ] == [ "2", "3", "4", "1" ]
                        , rotate 1 [] == []
                        ]
