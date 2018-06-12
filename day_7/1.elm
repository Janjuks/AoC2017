module Main exposing (..)

import Html exposing (..)
import Regex exposing (..)
import Day_7.Input exposing (puzzleInput)


type Program
    = Program
        { name : String
        , weight : Int
        , childrenProgramNames : List String
        , childrenPrograms : List Program
        }


testInput : String
testInput =
    """
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""


main : Html a
main =
    text rootProgramName


programsWithoutLeaves : List Program
programsWithoutLeaves =
    programs
        |> List.filter
            (\(Program p) -> List.length p.childrenPrograms /= 0)


rootProgramName : String
rootProgramName =
    programs
        |> List.filter (isProgramChildOfAnother >> not)
        |> List.head
        |> Maybe.map (\(Program p) -> p.name)
        |> Maybe.withDefault "hmmm"


isProgramChildOfAnother : Program -> Bool
isProgramChildOfAnother (Program program) =
    programs
        |> List.any (\(Program p) -> List.member program.name p.childrenProgramNames)


programs : List Program
programs =
    puzzleInput
        |> String.trim
        |> String.lines
        |> List.map inputLineToProgram
        |> \programs_ -> List.map (assignChildrenPrograms programs_) programs_


assignChildrenPrograms : List Program -> Program -> Program
assignChildrenPrograms programs (Program program) =
    Program
        { program
            | childrenPrograms =
                program.childrenProgramNames
                    |> List.map (findChildrenProgram programs)
        }


findChildrenProgram : List Program -> String -> Program
findChildrenProgram programs programName =
    let
        program =
            programs
                |> List.filter (\(Program p) -> p.name == programName)
                |> List.head
    in
        case program of
            Just val ->
                val

            Nothing ->
                Debug.crash "program not found"


inputLineToProgram : String -> Program
inputLineToProgram inputLine =
    Program
        { name =
            inputLine
                |> Regex.find All regexes.name
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""
        , weight =
            inputLine
                |> Regex.find All regexes.weight
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.withDefault 0
        , childrenProgramNames =
            inputLine
                |> Regex.find All regexes.childrenProgramNames
                |> List.head
                |> Maybe.map .submatches
                |> Maybe.andThen List.head
                |> Maybe.withDefault Maybe.Nothing
                |> Maybe.withDefault ""
                |> String.split ", "
                |> List.filter (String.isEmpty >> not)
        , childrenPrograms = []
        }


regexes : { name : Regex, weight : Regex, childrenProgramNames : Regex }
regexes =
    { name = regex "^\\w+"
    , weight = regex "\\d+"
    , childrenProgramNames = regex "->\\s(.+)"
    }
