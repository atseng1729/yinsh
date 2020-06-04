module Helper exposing (..)

import Dict exposing (Dict)
import Collage exposing (Point)

import Constants exposing (..)

-- TODO: change name to isEmptyHex
isEmptyHex : Dict IntPoint VState -> IntPoint -> Bool
isEmptyHex boardData p =
  let
    curVState = Dict.get p boardData
  in
    (curVState /= Nothing) && (curVState == Just None)

otherP : Player -> Player
otherP p = case p of 
  P1 -> P2
  P2 -> P1