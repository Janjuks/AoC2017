module Main exposing (..)

import Html exposing (..)
import Array exposing (..)
import Day_5.Input exposing (puzzleInput)


main : Html a
main =
    stepsToExit puzzleInput 0 0
        |> toString
        |> text


stepsToExit : Array Int -> Int -> Int -> Int
stepsToExit offsets currentIndex steps =
    case (Array.get currentIndex offsets) of
        Nothing ->
            steps

        Just offset ->
            let
                offsetIncrement =
                    if offset >= 3 then
                        -1
                    else
                        1

                updatedOffsets =
                    Array.set currentIndex (offset + offsetIncrement) offsets
            in
                stepsToExit updatedOffsets (currentIndex + offset) (steps + 1)
