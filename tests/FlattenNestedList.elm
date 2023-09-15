module FlattenNestedList exposing (..)

import Expect
import Problems.P7FlattenNestedList exposing (NestedList(..), flatten)
import Test exposing (..)


suite : Test
suite =
    test "Test" <|
        \_ ->
            Expect.equal 0 <|
                List.length <|
                    List.filter ((==) False)
                        [ flatten
                            (SubList
                                [ Elem 1
                                , SubList [ SubList [ Elem 2, SubList [ Elem 3, Elem 4 ] ], Elem 5 ]
                                , Elem 6
                                , SubList [ Elem 7, Elem 8, Elem 9 ]
                                ]
                            )
                            == List.range 1 9
                        , flatten (SubList [ Elem 1, Elem 2 ]) == [ 1, 2 ]
                        , flatten (SubList [ Elem "a", Elem "b" ]) == [ "a", "b" ]
                        , flatten (SubList []) == []
                        ]
