module SolutionTests.P39PrimesInRange exposing (suite)

import Expect
import Solutions.P39PrimesInRange exposing (primesInRange)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal 0
                    (List.length <|
                        List.filter ((==) False)
                            [ primesInRange 1 36 == [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 ]
                            , primesInRange 1 10 == [ 2, 3, 5, 7 ]
                            , primesInRange -1 1 == []
                            , primesInRange 1 1 == []
                            , primesInRange 100 1 == []
                            , primesInRange 0 1 == []
                            , primesInRange 60 100 == [ 61, 67, 71, 73, 79, 83, 89, 97 ]
                            , primesInRange 4 10 == [ 5, 7 ]
                            , primesInRange 24 100 == [ 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
                            ]
                    )
        ]
