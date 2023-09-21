module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (listToString)


suite : Test
suite =
    describe "Test list to string conversion"
        [ test "Returns empty brackets from empty list" <|
            \_ -> Expect.equal "[]" (listToString String.fromInt ", " [])
        , test "Returns single element" <|
            \_ -> Expect.equal "[42]" (listToString String.fromInt ", " [ 42 ])
        , test "Returns multiple elements" <|
            \_ -> Expect.equal "[42, 43, -44, 0]" (listToString String.fromInt ", " [ 42, 43, -44, 0 ])
        , test "Works recursively" <|
            \_ ->
                Expect.equal "[[1, 2, 3], [10, 20, 30]]"
                    (listToString (listToString String.fromInt ", ") ", " [ [ 1, 2, 3 ], [ 10, 20, 30 ] ])
        ]
