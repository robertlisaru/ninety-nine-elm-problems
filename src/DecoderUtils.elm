module DecoderUtils exposing (decodeDuplicates, nestedListDecoder, rleDecoder)

import Array
import Json.Decode as Decode exposing (Decoder)
import Solutions.P11RleEncode exposing (RleCode(..))
import Solutions.P7FlattenNestedList exposing (NestedList(..))


nestedListDecoder : Decoder (NestedList Int)
nestedListDecoder =
    Decode.oneOf
        [ Decode.int |> Decode.map Elem
        , Decode.list (Decode.lazy (\_ -> nestedListDecoder)) |> Decode.map SubList
        ]


decodeDuplicates : Decoder (List (List Int))
decodeDuplicates =
    let
        failIfListHasDifferentElements list =
            case list of
                first :: _ ->
                    if list /= List.repeat (List.length list) first then
                        Decode.fail "Invalid input"

                    else
                        Decode.succeed list

                [] ->
                    Decode.succeed list
    in
    Decode.list
        (Decode.list Decode.int |> Decode.andThen failIfListHasDifferentElements)


rleDecoder : Decoder (RleCode Int)
rleDecoder =
    Decode.string
        |> Decode.andThen
            (\rleString ->
                let
                    tokens =
                        rleString
                            |> String.split " "
                            |> List.filter (\token -> token /= "")
                            |> Array.fromList

                    rleType =
                        tokens |> Array.get 0
                in
                case rleType of
                    Just "Single" ->
                        if Array.length tokens /= 2 then
                            Decode.fail "invalid rle single"

                        else
                            let
                                singleElement =
                                    tokens
                                        |> Array.get 1
                                        |> Maybe.withDefault ""
                                        |> String.toInt
                            in
                            case singleElement of
                                Just element ->
                                    Decode.succeed (Single element)

                                _ ->
                                    Decode.fail "invalid rle single"

                    Just "Run" ->
                        if Array.length tokens /= 3 then
                            Decode.fail "invalid rle run"

                        else
                            let
                                runLength =
                                    tokens
                                        |> Array.get 1
                                        |> Maybe.withDefault ""
                                        |> String.toInt

                                repeatedElement =
                                    tokens
                                        |> Array.get 2
                                        |> Maybe.withDefault ""
                                        |> String.toInt
                            in
                            case ( runLength, repeatedElement ) of
                                ( Just runLength_, Just repeatedElement_ ) ->
                                    Decode.succeed (Run runLength_ repeatedElement_)

                                _ ->
                                    Decode.fail "invalid rle run"

                    _ ->
                        Decode.fail "Invalid rle type"
            )
