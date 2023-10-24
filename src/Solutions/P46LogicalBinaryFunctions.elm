module Solutions.P46LogicalBinaryFunctions exposing (and, equivalent, implies, nand, nor, or, truthTable, xor)


and : Bool -> Bool -> Bool
and a b =
    a && b


or : Bool -> Bool -> Bool
or a b =
    a || b


nand : Bool -> Bool -> Bool
nand a b =
    not (a && b)


nor : Bool -> Bool -> Bool
nor a b =
    not (a || b)


xor : Bool -> Bool -> Bool
xor a b =
    a /= b


implies : Bool -> Bool -> Bool
implies a b =
    not a || b


equivalent : Bool -> Bool -> Bool
equivalent a b =
    a == b


truthTable : (Bool -> Bool -> Bool) -> List ( Bool, Bool, Bool )
truthTable logicalFunction =
    [ ( True, True, logicalFunction True True )
    , ( True, False, logicalFunction True False )
    , ( False, True, logicalFunction False True )
    , ( False, False, logicalFunction False False )
    ]
