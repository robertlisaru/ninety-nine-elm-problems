module SolutionTests.P3ElementAt exposing (suite)

import Expect
import Solutions.P3ElementAt exposing (elementAt)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 3 - Element at"
        [ test "Returns nothing from empty list" <|
            \_ -> Expect.equal Nothing (elementAt 0 [])
        , test "Returns nothing from empty list again" <|
            \_ -> Expect.equal Nothing (elementAt -1 [])
        , test "Returns nothing from empty list one more time" <|
            \_ -> Expect.equal Nothing (elementAt 2 [])
        , test "Returns nothing for index outside list" <|
            \_ -> Expect.equal Nothing (elementAt 2 [ 1 ])
        , test "Returns nothing for index outside list again" <|
            \_ -> Expect.equal Nothing (elementAt -1 [ 1, 2, 3, 4 ])
        , test "Returns nothing for index outside list one more time" <|
            \_ -> Expect.equal Nothing (elementAt 0 [ 1, 2, 3, 4 ])
        , test "Happy case 1" <|
            \_ -> Expect.equal (Just 2) (elementAt 2 [ 1, 2, 3, 4 ])
        , test "Happy case 2" <|
            \_ -> Expect.equal (Just 1) (elementAt 1 [ 1 ])
        , test "Happy case 3" <|
            \_ -> Expect.equal (Just 'b') (elementAt 2 [ 'a', 'b', 'c' ])
        ]
