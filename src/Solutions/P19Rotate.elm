module Solutions.P19Rotate exposing (rotate)

import List


rotate : Int -> List a -> List a
rotate rot list =
    if list == [] then
        []

    else
        let
            rotMod =
                rot |> modBy (list |> List.length)

            leftSegment =
                list |> List.drop rotMod

            rightSegment =
                list |> List.take rotMod
        in
        leftSegment ++ rightSegment
