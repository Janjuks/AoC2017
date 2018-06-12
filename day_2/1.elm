module Main exposing (..)

import Html exposing (..)
import List exposing (..)
import String exposing (..)
import Day_2.Input exposing (puzzleInput)


main : Html a
main =
    puzzleInput
        |> String.trim
        |> String.lines
        |> List.map String.words
        |> List.map stringArrayToIntArray
        |> List.map rowDistance
        |> List.sum
        |> toString
        |> text


stringArrayToIntArray : List String -> List Int
stringArrayToIntArray stringArray =
    stringArray
        |> List.map
            (\str ->
                str
                    |> String.toInt
                    |> Result.withDefault -1
            )


rowDistance : List Int -> Int
rowDistance row =
    let
        max =
            row
                |> List.maximum
                |> Maybe.withDefault 0

        min =
            row
                |> List.minimum
                |> Maybe.withDefault 0
    in
        max - min