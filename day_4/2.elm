module Main exposing (..)

import Html exposing (..)
import Array
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

        hasAnagram =
            findIfHasAnagram words word
    in
        case word of
            Just val ->
                if hasAnagram then
                    False
                else
                    validPhrase words (index + 1)

            Nothing ->
                True


findIfHasAnagram : List String -> Maybe String -> Bool
findIfHasAnagram words word =
    let
        splitWord =
            case word of
                Just val ->
                    val
                        |> String.split ""
                        |> List.sort

                Nothing ->
                    []
    in
        case word of
            Just val ->
                words
                    |> List.filter
                        (\item ->
                            item
                                |> String.split ""
                                |> List.sort
                                |> (==) splitWord
                        )
                    |> List.length
                    |> (<) 1

            Nothing ->
                False
