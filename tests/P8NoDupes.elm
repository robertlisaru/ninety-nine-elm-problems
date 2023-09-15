module P8NoDupes exposing (suite)

import Expect
import Problems.P8NoDupes exposing (noDupes)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ noDupes [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ 1, 2, 5, 2, 1 ]
                        , noDupes [ 2, 1, 1, 1 ] == [ 2, 1 ]
                        , noDupes [ 2, 2, 2, 1, 1, 1 ] == [ 2, 1 ]
                        , noDupes [ 1 ] == [ 1 ]
                        , noDupes [] == []
                        , noDupes [ "aa", "aa", "aa" ] == [ "aa" ]
                        , noDupes [ "aab", "b", "b", "aa" ] == [ "aab", "b", "aa" ]
                        ]
