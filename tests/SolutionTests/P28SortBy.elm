module SolutionTests.P28SortBy exposing (suite)

import Expect
import Solutions.P28SortBy exposing (sortByLengthFrequency, sortByListLengths)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Problem 28 - Sort by"
        [ test "a" <|
            \_ ->
                Expect.equal 0
                    (List.length <|
                        List.filter ((==) False)
                            [ List.map List.length
                                (sortByListLengths
                                    [ []
                                    , [ 1 ]
                                    , List.range 1 2
                                    , List.range 1 3
                                    , List.range 1 4
                                    , List.range 1 5
                                    ]
                                )
                                == [ 0, 1, 2, 3, 4, 5 ]
                            , List.map List.length (sortByListLengths [ [] ])
                                == [ 0 ]
                            , List.map List.length
                                (sortByListLengths
                                    [ []
                                    , [ 1 ]
                                    , List.range 1 100000
                                    , List.range 1 4
                                    , List.range 1 3
                                    , List.range 1 2
                                    ]
                                )
                                == [ 0, 1, 2, 3, 4, 100000 ]
                            , List.map List.length (sortByListLengths [ [ 14 ], [ 15 ], [], [ 1 ], [ 12 ], [ 13 ] ])
                                == [ 0, 1, 1, 1, 1, 1 ]
                            , List.map List.length (sortByListLengths [ [ "a", "b", "c" ], [ "a", "b" ], [ "a" ] ])
                                == [ 1, 2, 3 ]
                            ]
                    )
        , test "b" <|
            \_ ->
                Expect.equal 0
                    (List.length <|
                        List.filter ((==) False)
                            [ (List.map List.length <|
                                sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6, 7, 8 ], [ 2, 34, 5 ], [] ]
                              )
                                == [ 0, 3, 3, 1, 1, 1 ]
                            , (List.map List.length <|
                                sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6 ], [ 2 ], List.range 1 100000 ]
                              )
                                == [ 100000, 1, 1, 1, 1, 1 ]
                            , (List.map List.length <|
                                sortByLengthFrequency [ [ 1, 2, 3 ], [ 6, 7, 8 ], [ 0 ], [ 2, 3, 5 ] ]
                              )
                                == [ 1, 3, 3, 3 ]
                            , (List.map List.length <|
                                sortByLengthFrequency [ [] ]
                              )
                                == [ 0 ]
                            ]
                    )
        ]
