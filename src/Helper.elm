module Helper exposing (..)

import Dict exposing (Dict)

import Constants exposing (..)

-- TODO: change name to isEmptyHex
isEmptyHex : Dict IntPoint VState -> IntPoint -> Bool
isEmptyHex boardData p =
  let
    curVState = Dict.get p boardData
  in
    (curVState == Just None)

-- Is there a ring?
isPlayerRing : Dict IntPoint VState -> IntPoint -> Player -> Bool
isPlayerRing boardData p player =
  let
    curVState = Dict.get p boardData
  in
    if player == Both then (curVState == Just (Ring P1)) || (curVState == Just (Ring P2))
    else curVState == Just (Ring player)

isPlayerMarker : Dict IntPoint VState -> IntPoint -> Player -> Bool
isPlayerMarker boardData p player =
  let
    curVState = Dict.get p boardData
  in
    if player == Both then (curVState == Just (Marker P1)) || (curVState == Just (Marker P2))
    else curVState == Just (Marker player)

getSign : Int -> Int
getSign x =
  if x > 0 then 1
  else if x == 0 then 0
  else -1

moveOneStep : IntPoint -> IntPoint -> IntPoint
moveOneStep (x1, y1) (x2, y2) =
  let
    xChange = getSign (x2 - x1)
    yChange = getSign (y2 - y1)
  in
    (x1 + xChange, y1 + yChange)

flipPointsBetween : IntPoint -> IntPoint -> Dict IntPoint VState -> Dict IntPoint VState
flipPointsBetween curP newRingP boardData =
  if not (isCollinear curP newRingP) then
    Debug.todo "flipPointsBetween - Should not reach here"
  else if curP == newRingP then
    boardData
  else
    case Dict.get curP boardData of
      Just (Marker player) ->
        let
          otherPlayer = otherP player
          newBoardData = Dict.insert curP (Marker otherPlayer) boardData
          newP = moveOneStep curP newRingP
        in
          flipPointsBetween newP newRingP newBoardData
      Just None ->
        let
          newP = moveOneStep curP newRingP
        in
          flipPointsBetween newP newRingP boardData
      _ ->
        Debug.todo "flipPointsBetween - Should not reach here"


-- returns True if no ring between curP and prevRingP and
-- curP immediately follows a contiguous line of markers
checkPointsBetween : Dict IntPoint VState -> IntPoint -> IntPoint -> Bool -> Bool
checkPointsBetween boardData curP prevRingP visitedEmptySpace =
  if curP == prevRingP then
    True
  else if isPlayerRing boardData curP Both then
    False
  else if visitedEmptySpace && (isPlayerMarker boardData curP Both) then
    False
  else
    let
      newP = moveOneStep curP prevRingP
      updatedVisitEmpty = visitedEmptySpace || (isEmptyHex boardData curP)
    in checkPointsBetween boardData newP prevRingP updatedVisitEmpty


isCollinear : IntPoint -> IntPoint -> Bool
isCollinear (x1, y1) (x2, y2) =
  (x1-x2 == y1-y2) || (x1 == x2) || (y1 == y2)

-- For move to be valid:
--   ring has to be placed in vacant space
--   ring must be collinear with original ring position
--   ring cannot jump over another ring
--   ring has to be placed immediately after a contiguous line of markers
isValidMove : Dict IntPoint VState -> IntPoint -> IntPoint -> Bool
isValidMove boardData p prevRingP =
  let
    curVState = Dict.get p boardData
    newP = moveOneStep p prevRingP
  in
    (curVState == Just None) &&
    (isCollinear p prevRingP) &&
    (checkPointsBetween boardData newP prevRingP False)

otherP : Player -> Player
otherP p = case p of
  P1   -> P2
  P2   -> P1
  Both -> Both
