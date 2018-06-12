module Main exposing (..)

import Html exposing (..)
import Array exposing (..)
import Day_6.Input exposing (puzzleInput)


main : Html a
main =
    puzzleInput
        |> String.words
        |> List.map
            (\val ->
                val
                    |> String.toInt
                    |> Result.withDefault -1
            )
        |> Array.fromList
        |> (\memBanks -> findRedistributionSteps memBanks Array.empty Array.empty 0)
        |> toString
        |> text


findRedistributionSteps : Array Int -> Array (Array Int) -> Array Int -> Int -> Int
findRedistributionSteps memBanks previousBankStates foundState steps =
    let
        ( biggestBankIndex, biggestBankValue ) =
            findBiggestMemoryBank memBanks

        clearedBanks =
            Array.set biggestBankIndex 0 memBanks

        redistributed =
            redistribute clearedBanks biggestBankValue (biggestBankIndex + 1)
    in
        if Array.length foundState == 0 then
            if checkBankStateRegistered previousBankStates redistributed then
                findRedistributionSteps redistributed Array.empty redistributed 0
            else
                findRedistributionSteps redistributed (Array.push redistributed previousBankStates) Array.empty (steps + 1)
        else
            if foundState == redistributed then
                steps + 1
            else
                findRedistributionSteps redistributed Array.empty foundState (steps + 1)


redistribute : Array Int -> Int -> Int -> Array Int
redistribute memBanks blocksRemaining index_ =
    let
        index =
            if index_ >= Array.length memBanks then
                0
            else
                index_

        bankValue =
            memBanks
                |> Array.get index
                |> Maybe.withDefault -1
    in
        if blocksRemaining > 0 then
            redistribute (Array.set index (bankValue + 1) memBanks) (blocksRemaining - 1) (index + 1)
        else
            memBanks


findBiggestMemoryBank : Array Int -> ( Int, Int )
findBiggestMemoryBank memBanks =
    memBanks
        |> Array.toIndexedList
        |> List.foldl
            (\( currentIndex, currentValue ) ( previousIndex, previousValue ) ->
                if currentValue > previousValue then
                    ( currentIndex, currentValue )
                else
                    ( previousIndex, previousValue )
            )
            ( -1, -1 )


checkBankStateRegistered : Array (Array Int) -> Array Int -> Bool
checkBankStateRegistered states state =
    states
        |> Array.filter (\state_ -> state_ == state)
        |> Array.length
        |> (<=) 1
