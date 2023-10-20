module SolutionTests.P40Goldbach exposing (suite)

import Expect
import Solutions.P31IsPrime exposing (isPrime)
import Solutions.P40Goldbach exposing (goldbach)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all (\n -> testGoldbachHappyCase n <| goldbach n)
                        [ 4, 10, 12, 14, 16, 18, 20, 100, 500, 999 ]
                    )
        , test "Test 2" <|
            \_ ->
                Expect.equal True
                    (List.all (\n -> goldbach n == Nothing) [ -9, -1, 0, 1, 11, 17 ])
        ]


testGoldbachHappyCase : Int -> Maybe ( Int, Int ) -> Bool
testGoldbachHappyCase n result =
    case result of
        Nothing ->
            False

        Just ( p1, p2 ) ->
            if p1 + p2 /= n then
                False

            else if not (isPrime p1 && isPrime p2) then
                False

            else
                True
