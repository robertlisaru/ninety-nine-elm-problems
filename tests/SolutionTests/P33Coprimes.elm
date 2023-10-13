module SolutionTests.P33Coprimes exposing (suite)

import Expect
import Solutions.P33Coprimes exposing (coprimes)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 33 - Coprimes"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all (\( result, expect ) -> result == expect)
                        [ ( coprimes 36 63, False )
                        , ( coprimes 10 25, False )
                        , ( coprimes 120 120, False )
                        , ( coprimes 2 12, False )
                        , ( coprimes 1313 1600, True )
                        , ( coprimes 23 37, True )
                        , ( coprimes 45 330, False )
                        , ( coprimes 24528 65934, False )
                        , ( coprimes 1600 1313, True )
                        , ( coprimes -23 37, True )
                        , ( coprimes 330 45, False )
                        , ( coprimes -23 37, True )
                        , ( coprimes -330 -45, False )
                        , ( coprimes -24528 65934, False )
                        ]
                    )
        ]
