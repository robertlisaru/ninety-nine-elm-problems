module Solutions.P46LogicalBinaryFunctions exposing (and, equivalent, implies, nand, nor, or, truthTable, xor)


and : Bool -> Bool -> Bool
and a b =
    -- your implementation here
    True



-- True if either a or b are true


or : Bool -> Bool -> Bool
or a b =
    -- your implementation here
    True



-- True either a or b are false


nand : Bool -> Bool -> Bool
nand a b =
    -- your implementation here
    True



-- True if and only if a and b are false


nor : Bool -> Bool -> Bool
nor a b =
    -- your implementation here
    True



-- True if a or b is true, but not if both are true


xor : Bool -> Bool -> Bool
xor a b =
    -- your implementation here
    True



-- True if a is false or b is true


implies : Bool -> Bool -> Bool
implies a b =
    -- your implementation here
    True



-- True if both a and b are true, or both a and b ar false


equivalent : Bool -> Bool -> Bool
equivalent a b =
    -- your implementation here
    True


truthTable : (Bool -> Bool -> Bool) -> List ( Bool, Bool, Bool )
truthTable f =
    -- your implementation goes here
    List.repeat 4 ( True, True, True )
