module Helper exposing (..)

import Dict exposing (Dict)
import Collage exposing (Point)

import Constants exposing (..)

isPlayer1 : Int -> Bool
isPlayer1 x =
  (modBy 2 x) == 0

isValidHex : Dict IntPoint VState -> IntPoint -> Bool
isValidHex boardData p =
  let
    curVState = Dict.get p boardData
  in
    (curVState /= Nothing) && (curVState /= Just R_Ring) && (curVState /= Just G_Ring)
