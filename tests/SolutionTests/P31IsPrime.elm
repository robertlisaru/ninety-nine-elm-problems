module SolutionTests.P31IsPrime exposing (..)

import Expect
import Solutions.P31IsPrime exposing (isPrime)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 31 - Is prime"
        [ test "a" <|
            \_ ->
                Expect.equal 0
                    (List.length <|
                        List.filter (\( result, expect ) -> result /= expect)
                            [ ( isPrime 36, False )
                            , ( isPrime 10, False )
                            , ( isPrime -1, False )
                            , ( isPrime 1, False )
                            , ( isPrime 0, False )
                            , ( isPrime 120, False )
                            , ( isPrime 2, True )
                            , ( isPrime 23, True )
                            , ( isPrime 6000, False )
                            , ( isPrime 7919, True )
                            , ( isPrime 7911, False )
                            , ( isPrime 63247, True )
                            , ( isPrime 63249, False )
                            ]
                    )
        ]
