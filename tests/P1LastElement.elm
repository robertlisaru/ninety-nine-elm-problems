module P1LastElement exposing (suite)

import Expect
import Solutions.P1LastElement exposing (last)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 1 - Last element"
        [ test "Returns nothing from empty list" <|
            \_ -> Expect.equal Nothing (last [])
        , test "Returns the element from single element list" <|
            \_ -> Expect.equal (Just 1) (last [ 1 ])
        , test "Returns the last element in rage" <|
            \_ -> Expect.equal (Just 4) (last <| List.range 1 4)
        , test "Returns the last element in list of characters" <|
            \_ -> Expect.equal (Just 'c') (last [ 'a', 'b', 'c' ])
        ]
