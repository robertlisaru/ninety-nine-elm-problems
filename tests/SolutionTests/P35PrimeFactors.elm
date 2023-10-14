module SolutionTests.P35PrimeFactors exposing (suite)

import Expect
import Solutions.P35PrimeFactors exposing (primeFactors)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal [ 2, 2, 3, 3 ] (primeFactors 36)
        , test "Test 2" <|
            \_ ->
                Expect.equal [ 2, 5 ] (primeFactors 10)
        , test "Test 3" <|
            \_ ->
                Expect.equal [] (primeFactors -1)
        , test "Test 4" <|
            \_ ->
                Expect.equal [] (primeFactors 1)
        , test "Test 5" <|
            \_ ->
                Expect.equal [] (primeFactors 0)
        , test "Test 6" <|
            \_ ->
                Expect.equal [ 2, 2, 2, 3, 5 ] (primeFactors 120)
        , test "Test 7" <|
            \_ ->
                Expect.equal [ 2 ] (primeFactors 2)
        , test "Test 8" <|
            \_ ->
                Expect.equal [ 23 ] (primeFactors 23)
        , test "Test 9" <|
            \_ ->
                Expect.equal [ 2, 7, 11, 449 ] (primeFactors 69146)
        , test "Test 10" <|
            \_ ->
                Expect.equal [ 9007 ] (primeFactors 9007)
        , test "Test 11" <|
            \_ ->
                Expect.equal [ 2, 2, 9007 ] (primeFactors 36028)
        , test "Test 12" <|
            \_ ->
                Expect.equal [ 2, 2, 3, 3, 3, 241 ] (primeFactors 26028)
        ]
