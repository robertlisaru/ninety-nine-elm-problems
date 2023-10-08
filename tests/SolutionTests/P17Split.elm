module SolutionTests.P17Split exposing (suite)

import Expect
import Solutions.P17Split exposing (split)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ split 0 (List.range 1 5) == ( [], [ 1, 2, 3, 4, 5 ] )
                        , split 2 (List.range 1 5) == ( [ 1, 2 ], [ 3, 4, 5 ] )
                        , split 3 (List.range 1 5) == ( [ 1, 2, 3 ], [ 4, 5 ] )
                        , split 4 (List.range 1 5) == ( [ 1, 2, 3, 4 ], [ 5 ] )
                        , split 5 (List.range 1 5) == ( [ 1, 2, 3, 4, 5 ], [] )
                        , split 6 (List.range 1 5) == ( [ 1, 2, 3, 4, 5 ], [] )
                        , split -1 (List.range 1 5) == ( [], [ 1, 2, 3, 4, 5 ] )
                        , split 2 [ "aab", "b", "c", "aa" ] == ( [ "aab", "b" ], [ "c", "aa" ] )
                        ]
