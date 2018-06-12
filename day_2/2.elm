module Main exposing (..)

import Html exposing (..)
import List exposing (..)
import Array exposing (..)
import String exposing (..)
import Day_2.Input exposing (puzzleInput)


main : Html a
main =
    puzzleInput
        |> String.trim
        |> String.lines
        |> List.map String.words
        |> List.map stringArrayToIntArray
        |> List.map Array.fromList
        |> List.map (findDivision 0 1)
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


findDivision : Int -> Int -> Array Int -> Int
findDivision index1 index2 row =
    let
        correctedIndex2 =
            if index1 == index2 then
                index2 + 1
            else
                index2

        item1 =
            row
                |> Array.get index1
                |> Maybe.withDefault -1

        item2 =
            row
                |> Array.get correctedIndex2
    in
        case item2 of
            Just val ->
                if rem item1 val == 0 then
                    item1 // val
                else if rem val item1 == 0 then
                    val // item1
                else
                    findDivision index1 (correctedIndex2 + 1) row

            Nothing ->
                findDivision (index1 + 1) 0 row
