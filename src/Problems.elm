module Problems exposing (Problem, problems)


type alias Problem =
    { number : Int
    , title : String
    , requirement : String
    }


problems : List Problem
problems =
    [ { number = 1, title = "Last element", requirement = "" }
    , { number = 2, title = "Penultimate", requirement = "" }
    , { number = 3, title = "Element at", requirement = "" }
    , { number = 4, title = "Count elements", requirement = "" }
    , { number = 5, title = "Reverse", requirement = "" }
    , { number = 6, title = "Is palindrome", requirement = "" }
    , { number = 7, title = "Flatten nested list", requirement = "" }
    , { number = 8, title = "No dupes", requirement = "" }
    , { number = 9, title = "Pack", requirement = "" }
    , { number = 10, title = "Run lengths", requirement = "" }
    , { number = 11, title = "Run lengths encode", requirement = "" }
    , { number = 12, title = "Run lengths decode", requirement = "" }
    , { number = 14, title = "Duplicate", requirement = "" }
    , { number = 15, title = "Repeat elements", requirement = "" }
    , { number = 16, title = "Drop nth", requirement = "" }
    , { number = 17, title = "Split", requirement = "" }
    , { number = 18, title = "Sublist", requirement = "" }
    , { number = 19, title = "Rotate", requirement = "" }
    , { number = 20, title = "Drop at", requirement = "" }
    , { number = 21, title = "Insert at", requirement = "" }
    , { number = 22, title = "Range", requirement = "" }
    , { number = 23, title = "Random select", requirement = "" }
    ]
