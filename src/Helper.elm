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

-- Sees if there is a ring at the value; return it if so
maybeRing : Dict IntPoint VState -> IntPoint -> Maybe VState
maybeRing boardData p = 
  case (Dict.get p boardData) of
    Just (Ring player) -> Just (Ring player)
    _ -> Nothing

-- Sees if there is a marker at the value; return it if so
maybeMarker : Dict IntPoint VState -> IntPoint -> Maybe VState
maybeMarker boardData p = 
  case (Dict.get p boardData) of
    Just (Marker player) -> Just (Marker player)
    _ -> Nothing

-- Is there a ring at the point?
isRing : Dict IntPoint VState -> IntPoint -> Bool
isRing boardData p  =
  case maybeRing boardData p of 
    Just _ -> True
    Nothing -> False 

-- Is there a ring of this player at the point??
isPlayerRing : Dict IntPoint VState -> IntPoint -> Player -> Bool
isPlayerRing boardData p player =
  case maybeRing boardData p of 
    Just (Ring rplayer) -> player == rplayer
    _ -> False 

-- Is there a marker at this point?
isMarker : Dict IntPoint VState -> IntPoint -> Bool
isMarker boardData p  =
  case maybeMarker boardData p of 
    Just _ -> True
    Nothing -> False 

-- Is there a marker of this player at this point?
isMarkerPlayer : Dict IntPoint VState -> IntPoint -> Player -> Bool
isMarkerPlayer boardData p player =
  case maybeMarker boardData p of 
    Just (Marker mplayer) -> player == mplayer
    _ -> False 

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
  else if isRing boardData curP then
    False
  else if visitedEmptySpace && (isMarker boardData curP) then
    False
  else
    let
      newP = moveOneStep curP prevRingP
      updatedVisitEmpty = visitedEmptySpace || (isEmptyHex boardData curP)
    in checkPointsBetween boardData newP prevRingP updatedVisitEmpty

-- checks is there a row between start and end for player P
-- ASSUMES start, end are collinear... we'll call isCollinear right now for debugging purposes; remove in prod
isRow : Dict IntPoint VState -> Player -> IntPoint -> IntPoint -> Bool 
isRow boardData pl start end =
  if not (isCollinear start end) then 
    Debug.todo "ERROR: points fed to isRow should be collinear..."
  else if (start == end) && (isMarkerPlayer boardData start pl) then 
    True 
  else 
    let nextpt = moveOneStep start end in 
    (isMarkerPlayer boardData start pl) && isRow boardData pl nextpt end

isCollinear : IntPoint -> IntPoint -> Bool
isCollinear (x1, y1) (x2, y2) =
  (x1-x2 == y1-y2) || (x1 == x2) || (y1 == y2)

-- Generate collinear points that are up to n away from the source! (Includes source point)
collinearPoints : Int -> IntPoint -> List IntPoint
collinearPoints n (x,y) =
  let range = List.range -n n 
      xpoints = List.map (\i -> (x + i, y)) range
      ypoints = List.map (\i -> (x, y+i)) range
      diagpoints = List.map (\i -> (x+i, y+i)) range 
  in xpoints ++ ypoints ++ diagpoints

-- For move to be valid:
--   ring has to be placed in vacant space
--   ring must be collinear with original ring position
--   ring cannot jump over another ring
--   ring has to be placed immediately after a contiguous line of markers

-- Returns whether move (prevRingP -> p) is valid 
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