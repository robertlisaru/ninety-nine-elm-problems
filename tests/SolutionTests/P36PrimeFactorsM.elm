module SolutionTests.P36PrimeFactorsM exposing (suite)

import Expect
import Solutions.P36PrimeFactorsM exposing (primeFactorsM)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all (\( result, expect ) -> result == expect)
                        [ ( primeFactorsM 36, [ ( 2, 2 ), ( 3, 2 ) ] )
                        , ( primeFactorsM 10, [ ( 2, 1 ), ( 5, 1 ) ] )
                        , ( primeFactorsM -1, [] )
                        , ( primeFactorsM 1, [] )
                        , ( primeFactorsM 0, [] )
                        , ( primeFactorsM 120, [ ( 2, 3 ), ( 3, 1 ), ( 5, 1 ) ] )
                        , ( primeFactorsM 2, [ ( 2, 1 ) ] )
                        , ( primeFactorsM 23, [ ( 23, 1 ) ] )
                        , ( primeFactorsM 69146, [ ( 2, 1 ), ( 7, 1 ), ( 11, 1 ), ( 449, 1 ) ] )
                        , ( primeFactorsM 9007, [ ( 9007, 1 ) ] )
                        , ( primeFactorsM 36028, [ ( 2, 2 ), ( 9007, 1 ) ] )
                        , ( primeFactorsM 26028, [ ( 2, 2 ), ( 3, 3 ), ( 241, 1 ) ] )
                        ]
                    )
        ]
