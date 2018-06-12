module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Day_1.Input exposing (puzzleInput)


main : Html a
main =
    text "asd"


res : Int
res =
    2


rgx : Regex
rgx =
    "(\\d)(\\1+)"
