module ProblemHeaders exposing (Category, ProblemHeader, categories, problemHeaders)


type alias ProblemHeader =
    { number : Int
    , title : String
    }


type alias Category =
    { title : String
    , problems : List ProblemHeader
    }


categories : List Category
categories =
    [ { title = "Lists"
      , problems =
            [ { number = 1, title = "Last element" }
            , { number = 2, title = "Penultimate" }
            , { number = 3, title = "Element at" }
            , { number = 4, title = "Count elements" }
            , { number = 5, title = "Reverse" }
            , { number = 6, title = "Is palindrome" }
            , { number = 7, title = "Flatten nested list" }
            , { number = 8, title = "No dupes" }
            , { number = 9, title = "Pack" }
            , { number = 10, title = "Run lengths" }
            , { number = 11, title = "Run lengths encode" }
            , { number = 12, title = "Run lengths decode" }
            , { number = 14, title = "Duplicate" }
            , { number = 15, title = "Repeat elements" }
            , { number = 16, title = "Drop nth" }
            , { number = 17, title = "Split" }
            , { number = 18, title = "Sublist" }
            , { number = 19, title = "Rotate" }
            , { number = 20, title = "Drop at" }
            , { number = 21, title = "Insert at" }
            , { number = 22, title = "Range" }
            , { number = 23, title = "Random select" }
            , { number = 24, title = "Lotto" }
            , { number = 26, title = "Combinations" }
            , { number = 28, title = "Sort by" }
            ]
      }
    , { title = "Arithmetic"
      , problems =
            [ { number = 31, title = "Is prime" }
            , { number = 32, title = "Greatest common divisor" }
            , { number = 33, title = "Coprimes" }
            , { number = 34, title = "Totient" }
            , { number = 35, title = "Prime factors" }
            , { number = 36, title = "Prime factors multiplicity" }
            , { number = 37, title = "Totient improved" }
            , { number = 38, title = "Benchmark totient" }
            , { number = 39, title = "Primes in range" }
            , { number = 40, title = "Goldbach's conjecture" }
            , { number = 41, title = "Goldbach threshold" }
            ]
      }
    , { title = "Logic and Codes"
      , problems =
            [ { number = 46, title = "Logical binary functions" }
            , { number = 47, title = "Infix operators" }
            , { number = 49, title = "Gray code" }
            , { number = 50, title = "Huffman encoding" }
            ]
      }
    , { title = "Binary Trees"
      , problems =
            [ { number = 54, title = "Binary tree" }
            , { number = 55, title = "Balanced tree" }
            , { number = 56, title = "Symmetric tree" }
            , { number = 57, title = "Binary search tree" }
            , { number = 61, title = "Count leaves" }
            , { number = 62, title = "Count internal nodes" }
            , { number = 63, title = "Complete tree" }
            , { number = 64, title = "Draw a tree" }
            , { number = 65, title = "Draw a tree (other layout)" }
            , { number = 67, title = "Tree to/from string" }
            , { number = 68, title = "Tree traversal" }
            , { number = 69, title = "Tree dot string representation" }
            ]
      }
    , { title = "Multi-way Trees"
      , problems =
            [ { number = 70, title = "Multiway tree" }
            , { number = 71, title = "Internal path length" }
            , { number = 72, title = "Depth first order" }
            , { number = 73, title = "Multiway tree to string" }
            ]
      }
    , { title = "Graphs"
      , problems =
            [ { number = 80, title = "Adjacency-list" }
            , { number = 81, title = "Find paths" }
            , { number = 84, title = "Minimal spanning tree" }
            , { number = 86, title = "Graph coloring" }
            , { number = 87, title = "Graph traversal" }
            , { number = 88, title = "Connected components" }
            ]
      }
    , { title = "Puzzles"
      , problems =
            [ { number = 90, title = "Queens" }
            , { number = 91, title = "The Knight's Tour" }
            , { number = 92, title = "Graceful labeling" }
            , { number = 93, title = "Equation" }
            , { number = 94, title = "Regular graphs" }
            , { number = 95, title = "Number spelling" }
            ]
      }
    ]


problemHeaders : List ProblemHeader
problemHeaders =
    categories |> List.concatMap .problems
