module Rotate exposing (test)

import List


rotate : Int -> List a -> List a
rotate rot list =
    if rot == 0 then
        list

    else if (list |> List.length) == 0 then
        []

    else
        let
            absRot =
                abs rot

            rotMod =
                absRot |> modBy length

            length =
                list |> List.length

            leftSegment =
                if rot < 0 then
                    list |> List.drop (length - rotMod)

                else
                    list |> List.drop rotMod

            rightSegment =
                if rot < 0 then
                    list |> List.take (length - rotMod)

                else
                    list |> List.take rotMod
        in
        leftSegment ++ rightSegment


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ rotate 3 [ 1, 2, 5, 5, 2, 1 ] == [ 5, 2, 1, 1, 2, 5 ]
            , rotate 13 (List.range 1 10) == [ 4, 5, 6, 7, 8, 9, 10, 1, 2, 3 ]
            , rotate 1 (List.range 1 5) == [ 2, 3, 4, 5, 1 ]
            , rotate 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , rotate -1 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
            , rotate -6 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
            , rotate 3 (List.range 1 5) == [ 4, 5, 1, 2, 3 ]
            , rotate 1 [ "1", "2", "3", "4" ] == [ "2", "3", "4", "1" ]
            , rotate 1 [] == []
            ]
