module P14Duplicate exposing (suite)

import Expect
import Problems.P14Duplicate exposing (duplicate)
import Test exposing (Test, test)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ duplicate [ 1, 2, 3, 5, 8, 8 ] == [ 1, 1, 2, 2, 3, 3, 5, 5, 8, 8, 8, 8 ]
                        , duplicate [] == []
                        , duplicate [ 1 ] == [ 1, 1 ]
                        , duplicate [ "1", "2", "5" ] == [ "1", "1", "2", "2", "5", "5" ]
                        ]
