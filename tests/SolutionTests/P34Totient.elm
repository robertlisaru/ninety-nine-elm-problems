module SolutionTests.P34Totient exposing (suite)

import Expect
import Solutions.P34Totient exposing (totient)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 34 - Totient"
        [ test "Test 1" <|
            \_ ->
                Expect.equal 0
                    (List.length <|
                        List.filter ((==) False)
                            [ totient 10 == 4
                            , totient 25 == 20
                            , totient 120 == 32
                            , totient 0 == 0
                            , totient 1600 == 640
                            , totient 37 == 36
                            , totient 330 == 80
                            , totient 65934 == 19440
                            , totient 1313 == 1200
                            , totient 45 == 24
                            , totient -23 == 0
                            , totient 1 == 1
                            ]
                    )
        ]
