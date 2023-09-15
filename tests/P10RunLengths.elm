module P10RunLengths exposing (suite)

import Expect
import Problems.P10RunLengths exposing (runLengths)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                            == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
                        , runLengths [ [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
                            == [ ( 1, 2 ), ( 2, 5 ), ( 1, 2 ), ( 1, 1 ) ]
                        , runLengths [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ] ]
                            == [ ( 4, 1 ), ( 1, 2 ), ( 2, 5 ) ]
                        , runLengths [ [ 1, 1, 1, 1 ] ]
                            == [ ( 4, 1 ) ]
                        , runLengths [ [ "a", "a", "a", "a" ], [ "b" ], [ "c", "c" ], [ "b" ], [ "a" ] ]
                            == [ ( 4, "a" ), ( 1, "b" ), ( 2, "c" ), ( 1, "b" ), ( 1, "a" ) ]
                        , runLengths [ [] ] == []
                        , runLengths [ [], [ "a", "a" ] ] == [ ( 2, "a" ) ]
                        , runLengths [] == []
                        ]
