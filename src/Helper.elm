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
    (curVState == Just None)

-- Is there a ring?
isRing : Dict IntPoint VState -> IntPoint -> Bool
isRing boardData p =
  let
    curVState = Dict.get p boardData
  in
    (curVState == Just (Ring P1)) || (curVState == Just (Ring P2))

-- Sees if there is a ring at the value; return it if so
maybeRing : Dict IntPoint VState -> IntPoint -> Maybe VState
maybeRing boardData p = 
  case (Dict.get p boardData) of
    Just (Ring player) -> Just (Ring player)
    _ -> Nothing

-- return a list of all valid moves of a ring
-- Assumes position given to it is a ring. throw error if not FOR DEBUG ONLY

-- My implementation idea: sketched below
-- Probably want to use recursive helpers with 3 directions, scanning a line until you hit a ring
-- to recursively generate lists, then combine them. seems not bad.

validMoves : Dict IntPoint VState -> IntPoint -> List IntPoint 
validMoves boardData p = 
  Debug.todo "arthur tseng you can start working on this soon cuz there's functionality till confirm"

otherP : Player -> Player
otherP p = case p of 
  P1 -> P2
  P2 -> P1