module SolutionTests.P26Combinations exposing (suite)

import Expect
import Solutions.P26Combinations exposing (combinations)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 26 - Combinations"
        [ test "Test 1" <|
            \_ ->
                Expect.equal [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ] <| combinations 1 (List.range 1 5)
        , test "Test 2" <|
            \_ ->
                Expect.equal [ [ 'a', 'b' ], [ 'a', 'c' ], [ 'b', 'c' ] ] <| combinations 2 [ 'a', 'b', 'c' ]
        , test "Test 3" <|
            \_ ->
                Expect.equal [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ] ] <| combinations 2 (List.range 1 3)
        , test "Test 4" <|
            \_ ->
                Expect.equal [ [ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 2, 3 ], [ 2, 4 ], [ 3, 4 ] ] <| combinations 2 (List.range 1 4)
        , test "Test 5" <|
            \_ ->
                Expect.equal [ [] ] <| combinations 0 (List.range 1 10)
        , test "Test 6" <|
            \_ ->
                Expect.equal [ [] ] <| combinations -1 (List.range 1 10)
        , test "Test 7" <|
            \_ ->
                Expect.equal 220 <| List.length (combinations 3 (List.range 1 12))
        , test "Test 8" <|
            \_ ->
                Expect.equal 1365 <| List.length (combinations 4 (List.range 1 15))
        ]
