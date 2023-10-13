module SolutionTests.P32GCD exposing (suite)

import Expect
import Solutions.P32GCD exposing (gcd)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 32 - Greatest common divisor"
        [ test "Test 1" <| \_ -> Expect.equal 9 (gcd 36 63)
        , test "Test 2" <| \_ -> Expect.equal 5 (gcd 10 25)
        , test "Test 3" <| \_ -> Expect.equal 120 (gcd 120 120)
        , test "Test 4" <| \_ -> Expect.equal 2 (gcd 2 12)
        , test "Test 5" <| \_ -> Expect.equal 1 (gcd 23 37)
        , test "Test 6" <| \_ -> Expect.equal 15 (gcd 45 330)
        , test "Test 7" <| \_ -> Expect.equal 6 (gcd 24528 65934)
        , test "Test 8" <| \_ -> Expect.equal 120 (gcd 120 -120)
        , test "Test 9" <| \_ -> Expect.equal 2 (gcd -2 12)
        , test "Test 10" <| \_ -> Expect.equal 1 (gcd 37 23)
        , test "Test 11" <| \_ -> Expect.equal 2 (gcd 2 -12)
        , test "Test 12" <| \_ -> Expect.equal 12 (gcd 0 -12)
        , test "Test 13" <| \_ -> Expect.equal 12 (gcd -12 0)
        , test "Test 14" <| \_ -> Expect.equal 12 (gcd -12 -12)
        ]
