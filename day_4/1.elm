module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import List exposing (..)
import Array exposing (..)
import Day_4.Input exposing (puzzleInput)


main : Html a
main =
    puzzleInput
        |> result
        |> toString
        |> text


result : String -> Int
result input =
    input
        |> String.trim
        |> String.lines
        |> List.map String.trim
        |> List.map String.words
        |> List.filter (flip validPhrase 0)
        |> List.length


validPhrase : List String -> Int -> Bool
validPhrase words index =
    let
        word =
            words
                |> Array.fromList
                |> Array.get index

        isOnlyWordInList =
            findIfOnlyWordInList words word
    in
        case word of
            Just val ->
                if isOnlyWordInList then
                    validPhrase words (index + 1)
                else
                    False

            Nothing ->
                True


findIfOnlyWordInList : List String -> Maybe String -> Bool
findIfOnlyWordInList words word =
    case word of
        Just val ->
            words
                |> List.filter (\item -> item == val)
                |> List.length
                |> (==) 1

        Nothing ->
            False
