module SolutionTests.P49GrayCodes exposing (..)

import Expect
import Solutions.P49GrayCodes exposing (grayCodes)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all ((==) True)
                        [ grayCodes 1 == [ [ 0 ], [ 1 ] ]
                        , grayCodes 2 == [ [ 0, 0 ], [ 0, 1 ], [ 1, 1 ], [ 1, 0 ] ]
                        , grayCodes 3 == [ [ 0, 0, 0 ], [ 0, 0, 1 ], [ 0, 1, 1 ], [ 0, 1, 0 ], [ 1, 1, 0 ], [ 1, 1, 1 ], [ 1, 0, 1 ], [ 1, 0, 0 ] ]
                        , grayCodes 4 == [ [ 0, 0, 0, 0 ], [ 0, 0, 0, 1 ], [ 0, 0, 1, 1 ], [ 0, 0, 1, 0 ], [ 0, 1, 1, 0 ], [ 0, 1, 1, 1 ], [ 0, 1, 0, 1 ], [ 0, 1, 0, 0 ], [ 1, 1, 0, 0 ], [ 1, 1, 0, 1 ], [ 1, 1, 1, 1 ], [ 1, 1, 1, 0 ], [ 1, 0, 1, 0 ], [ 1, 0, 1, 1 ], [ 1, 0, 0, 1 ], [ 1, 0, 0, 0 ] ]
                        ]
                    )
        ]
