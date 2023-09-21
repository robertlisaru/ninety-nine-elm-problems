module SolutionTests.P2Penultimate exposing (suite)

import Expect
import Solutions.P2Penultimate exposing (penultimate)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 2 - Penultimate"
        [ test "Returns nothing from empty list" <|
            \_ -> Expect.equal Nothing (penultimate [])
        , test "Returns nothing from single element list" <|
            \_ -> Expect.equal Nothing (penultimate [ 1 ])
        , test "Returns nothing from single element list of char" <|
            \_ -> Expect.equal Nothing (penultimate [ "a" ])
        , test "Returns the penultimate element in list of numbers" <|
            \_ -> Expect.equal (Just 3) (penultimate [ 1, 2, 3, 4 ])
        , test "Returns the penultimate element in list of 2 numbers" <|
            \_ -> Expect.equal (Just 1) (penultimate [ 1, 2 ])
        , test "Returns the penultimate element in list of characters" <|
            \_ -> Expect.equal (Just 'b') (penultimate [ 'a', 'b', 'c' ])
        ]
