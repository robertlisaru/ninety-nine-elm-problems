module SolutionTests.P46LogicalBinaryFunctions exposing (suite)

import Expect
import Solutions.P46LogicalBinaryFunctions exposing (and, equivalent, implies, nand, nor, or, truthTable)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Test suite"
        [ test "Test 1" <|
            \_ ->
                Expect.equal True
                    (List.all ((==) True)
                        [ truthTable and
                            == [ ( True, True, True )
                               , ( True, False, False )
                               , ( False, True, False )
                               , ( False, False, False )
                               ]
                        , truthTable or
                            == [ ( True, True, True )
                               , ( True, False, True )
                               , ( False, True, True )
                               , ( False, False, False )
                               ]
                        , truthTable nand
                            == [ ( True, True, False )
                               , ( True, False, True )
                               , ( False, True, True )
                               , ( False, False, True )
                               ]
                        , truthTable nor
                            == [ ( True, True, False )
                               , ( True, False, False )
                               , ( False, True, False )
                               , ( False, False, True )
                               ]
                        , truthTable Solutions.P46LogicalBinaryFunctions.xor
                            == [ ( True, True, False )
                               , ( True, False, True )
                               , ( False, True, True )
                               , ( False, False, False )
                               ]
                        , truthTable implies
                            == [ ( True, True, True )
                               , ( True, False, False )
                               , ( False, True, True )
                               , ( False, False, True )
                               ]
                        , truthTable equivalent
                            == [ ( True, True, True )
                               , ( True, False, False )
                               , ( False, True, False )
                               , ( False, False, True )
                               ]
                        , truthTable (\a b -> and a (or a b))
                            == [ ( True, True, True )
                               , ( True, False, True )
                               , ( False, True, False )
                               , ( False, False, False )
                               ]
                        ]
                    )
        ]
