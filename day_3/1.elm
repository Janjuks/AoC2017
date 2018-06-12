module Main exposing (..)

import Html exposing (..)
import Day_3.Input exposing (puzzleInput)


main : Html a
main =
    distance puzzleInput
        |> toString
        |> text


distance : Int -> Int
distance number =
    let
        root =
            findLayerRoot number

        highestNumber =
            root ^ 2

        layerIndex =
            findLayerIndex root

        maxDistanceInLayer =
            (root - 1) // 2

        distanceInLayer =
            findDistanceFromLayerCenter maxDistanceInLayer maxDistanceInLayer number highestNumber
    in
        distanceInLayer + layerIndex


findDistanceFromLayerCenter : Int -> Int -> Int -> Int -> Int
findDistanceFromLayerCenter currentIndex maxIndex number currentNumber =
    if number == currentNumber then
        abs currentIndex
    else if negate currentIndex == maxIndex then
        findDistanceFromLayerCenter (maxIndex - 1) maxIndex number (currentNumber - 1)
    else
        findDistanceFromLayerCenter (currentIndex - 1) maxIndex number (currentNumber - 1)


findLayerRoot : Int -> Int
findLayerRoot number =
    -- square root of highest number in layer
    let
        res =
            number
                |> toFloat
                |> sqrt
                |> ceiling
    in
        if isOdd res then
            res
        else
            res + 1


findLayerIndex : Int -> Int
findLayerIndex layerRoot =
    layerRoot
        |> toFloat
        |> flip (/) 2
        |> ceiling
        |> (-) layerRoot
        |> abs


isOdd : Int -> Bool
isOdd number =
    rem number 2 /= 0
