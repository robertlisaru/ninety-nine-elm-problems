module ElementCount exposing (test)


countElements : List a -> Int
countElements list =
    list
        |> List.map (\_ -> 1)
        |> List.sum


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ countElements (List.range 1 4000) == 4000
            , countElements [ 1 ] == 1
            , countElements [] == 0
            , countElements [ 'a', 'b', 'c' ] == 3
            ]
