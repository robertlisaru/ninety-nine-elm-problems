module SolutionTests.P41GoldbachThreshold exposing (suite)

import Expect
import Solutions.P41GoldbachThreshold exposing (goldbachGT)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all ((==) True)
                        [ goldbachGT 1 200 12 == [ ( 19, 79 ), ( 13, 109 ), ( 13, 113 ), ( 19, 109 ) ]
                        , goldbachGT (73 + 919) (73 + 919) 50 == [ ( 73, 919 ) ]
                        , goldbachGT 1 200 19 == []
                        , goldbachGT 0 -10 19 == []
                        ]
                    )
        ]
