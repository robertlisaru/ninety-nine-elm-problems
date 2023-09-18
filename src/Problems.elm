module Problems exposing (Problem, problems)


type alias Problem =
    { identifier : String
    , title : String
    , requirement : String
    }


problems : List Problem
problems =
    [ { identifier = "1", title = "Last element", requirement = "" }
    , { identifier = "2", title = "Penultimate", requirement = "" }
    , { identifier = "3", title = "Element at", requirement = "" }
    , { identifier = "4", title = "Count elements", requirement = "" }
    , { identifier = "5", title = "Reverse", requirement = "" }
    , { identifier = "6", title = "Is palindrome", requirement = "" }
    , { identifier = "7", title = "Flatten nested list", requirement = "" }
    , { identifier = "8", title = "No dupes", requirement = "" }
    , { identifier = "9", title = "Pack", requirement = "" }
    , { identifier = "10", title = "Run lengths", requirement = "" }
    , { identifier = "11", title = "Run lengths encode", requirement = "" }
    , { identifier = "12", title = "Run lengths decode", requirement = "" }
    , { identifier = "14", title = "Duplicate", requirement = "" }
    , { identifier = "15", title = "Repeat elements", requirement = "" }
    , { identifier = "16", title = "Drop nth", requirement = "" }
    , { identifier = "17", title = "Split", requirement = "" }
    , { identifier = "18", title = "Sublist", requirement = "" }
    , { identifier = "19", title = "Rotate", requirement = "" }
    , { identifier = "20", title = "Drop at", requirement = "" }
    , { identifier = "21", title = "Insert at", requirement = "" }
    , { identifier = "22", title = "Range", requirement = "" }
    , { identifier = "23", title = "Random select", requirement = "" }
    ]
