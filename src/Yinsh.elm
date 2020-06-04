-- For debugging purposes, exposing everything
module Yinsh exposing (..)

import Browser
import Browser.Events as E

import Json.Decode as D

import Html exposing (..)
import Html.Attributes as Attr
import Debug
import Time
import Dict exposing (Dict)

import Collage exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)

import Constants exposing (..)
import Helper exposing (..)

-- Game state; add more to this as we think of more reasonable states
-- PlaceR -> Placing the rings initially
-- SelectR -> selecting ring to move
-- Confirm -> choosing the move out of the possible ones
-- RemoveR -> remove a ring upon 5-in-row (or many if 2 5s are formed at once)
-- Win -> Display win message
-- True/False to toggle which side is moving
-- Colors: R, G
type GState = PlaceR Player | SelectR Player | Confirm Player | RemoveR Player Int | Win Player

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { boardData : Dict IntPoint VState
  , gameState : GState
  , score : (Int, Int)
  , windowWidth : Float
  , windowHeight : Float
  , mouseHex : IntPoint     -- Hex coordinates of mouse, use this!
  , selectMouseHex : IntPoint -- Hex coordinates of mouse in last state, used for confirm + other stuff maybe in future
  , p1Rings : Int          -- Current Number of Red Rings on Board
  , p2Rings : Int
  , validMoves : List IntPoint -- List of valid coordinates that can be moved to; only used currently for drawing possible moves
  }

type alias Flags =
  { windowWidth : Int
  , windowHeight : Int
  }

type Msg = WindowResize Int Int |
           MouseMoved IntPoint |
           MouseClick IntPoint |
           UpdateValidPts -- trigger message to recalculate valid points and update model accordingly

initModel : Flags -> Model
initModel flags = {boardData = emptyBoard,
            gameState = PlaceR P1,
            score = (0, 0),
            windowWidth = toFloat flags.windowWidth,
            windowHeight = toFloat flags.windowHeight,
            mouseHex = (0, 0),
            selectMouseHex = (0, 0),
            p1Rings = 0,
            p2Rings = 0, 
            validMoves = []}

-- Given model, color, and change,
changeRings : Model -> Player -> Int -> Model
changeRings model p n =
  case p of
    P1 -> {model | p1Rings = model.p1Rings + n}
    P2 -> {model | p2Rings = model.p2Rings + n}

addRing model player = changeRings model player 1
removeRing model player = changeRings model player -1

init : Flags -> (Model, Cmd Msg)
init flags =
  (initModel flags, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  -- Add code to differentiate behavior in different states! TODO
  Sub.batch [
    E.onResize WindowResize,
    -- map : (a -> value) -> Decoder a -> Decoder value. remove later but helpful now
    E.onMouseMove (decodeMouse model False),
    E.onMouseDown (decodeMouse model True)
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- WindowResize has carries two ints next to it. NOTE: VALIDPOINTS IS NOT WELL KEPT TRACK OF RN
    UpdateValidPts ->
      let candidates = collinearPoints 9 model.selectMouseHex 
          valids = List.filter (\pt -> isValidMove model.boardData pt model.selectMouseHex) candidates
      in ({model | validMoves = valids}, Cmd.none)

    WindowResize x y -> ({model | windowWidth = toFloat x, windowHeight = toFloat y}, Cmd.none)

    MouseMoved pt ->
      ({model | mouseHex = pt}, Cmd.none)

    MouseClick pt ->
      case model.gameState of
        PlaceR player ->
          if isEmptyHex model.boardData pt then
            let
              other = otherP player
              -- So that when there are 9 rings on the board, the appropriate state change will be triggered after placing the 10th
              newGState = if (model.p1Rings + model.p2Rings) < 9 then PlaceR other else SelectR other
              newBoardData = Dict.insert pt (Ring player) model.boardData
              newModel = {model | boardData = newBoardData, gameState = newGState}
            in
              (addRing newModel player, Cmd.none)
          else
            (model, Cmd.none)
        SelectR player ->
          if isPlayerRing model.boardData pt player then -- Process the new valid moves for display in confirm
            update UpdateValidPts {model | gameState = Confirm player, selectMouseHex = model.mouseHex}
          else
            (model, Cmd.none)
        Confirm player ->
          if isValidMove model.boardData pt model.selectMouseHex then
            let
              otherPlayer = otherP player
              newGState = SelectR otherPlayer
              curP = moveOneStep model.selectMouseHex pt
              newBoardData = model.boardData
                |> Dict.insert pt (Ring player)
                |> Dict.insert model.selectMouseHex (Marker player)
                |> flipPointsBetween curP pt
            in
              ({model | gameState = newGState, boardData = newBoardData}, Cmd.none)
          else -- if click on invalid point, then transition back to select phase
            ({model | gameState = SelectR player, selectMouseHex = model.mouseHex},
              Cmd.none)

        _ -> Debug.todo "Need to determine other cases for mouse click"

--------------------------
-- see https://github.com/elm/browser/blob/1.0.2/examples/src/Drag.elm for example of decoder interaction

-- Decoder for json -> Msg that contains x, y coords of mouse from JSON
decodeMouse : Model -> Bool -> D.Decoder Msg
decodeMouse model isClick =
  let
    decode_coords = D.map2 (\x y -> (x,y)) (D.field "clientX" D.float) (D.field "clientY" D.float)
    p = D.map (mouseInputToHex model) decode_coords
    msg_type = if isClick then MouseClick else MouseMoved
  in
    (D.map msg_type p)

--------------------------
-- Converts x,y from javascript to IntPoint
-- Center at middle of canvas and then invert y-axis
mouseInputToHex : Model -> Point -> IntPoint
mouseInputToHex model (mx, my) =
  (mx - model.windowWidth/2, model.windowHeight/2 - my) |> pix2hex

-- Renders a single ring at the given hex coordinates
drawRing : IntPoint -> Player -> Collage Msg
drawRing p player =
  let
    (cx, cy) = hex2pix p
    ringColor = if (player == P1) then p1Color else p2Color
  in
    (circle ring_size)
      |> outlined (solid thick (uniform ringColor))
      |> shift (cx, cy)

drawMarker : IntPoint -> Player -> Collage Msg
drawMarker p player =
  let
    (cx, cy) = hex2pix p
    markerColor = if (player == P1) then p1Color else p2Color
  in
    (circle marker_size)
      |> filled (uniform markerColor)
      |> shift (cx, cy)

-- drawDot; just draw a smaller black dot
drawDot : IntPoint -> Collage Msg 
drawDot p = 
  (circle (marker_size * 0.6)) |> filled (uniform dotColor) |> shift (hex2pix p)


renderBoard : List (Point, Point) -> Collage Msg
renderBoard edges_coords =
  let edges = List.map (\(p1, p2) -> segment p1 p2
              |> traced (solid thin (uniform boardColor))) edges_coords
              |> group
      -- Need a border b/c of the glitch near the edges. Also looks better
      border = square (10 * side) |> outlined (solid thin (uniform borderColor))
  in
  group [border, edges]

renderPiece : (IntPoint, VState) -> Collage Msg
renderPiece (p, state) =
  case state of
    Marker player -> drawMarker p player
    Ring player -> drawRing p player
    None     -> Debug.todo "renderPiece - should not reach here"

-- Used to add extra dots for valid move markers
renderDots : List (IntPoint) -> Collage Msg
renderDots ps = 
  group <| List.map (\p -> drawDot p) ps

renderPieces : Dict IntPoint VState -> Collage Msg
renderPieces boardData =
  (Dict.toList boardData)
    |> List.filter (\(p, state) -> state /= None)
    |> List.map renderPiece
    |> group

-- Master function to show DYNAMIC graphics; STATIC graphics are drawn via view!
-- Adds a floating ring if appropriate on top of the mouse cursor; should be
-- final preprocessing step before drawing collage
addFloatingElems : Model -> Collage Msg -> Collage Msg
addFloatingElems model canvas =
  case model.gameState of
    PlaceR player ->
      if (isEmptyHex model.boardData model.mouseHex) then
        group [drawRing model.mouseHex player, canvas]
      else canvas
    SelectR player ->
      if isPlayerRing model.boardData model.mouseHex player then
        group [drawMarker model.mouseHex player, canvas]
      else
        canvas
    Confirm player ->
      let
        dots = renderDots model.validMoves
        canvasWithMarker = group [dots, drawMarker model.selectMouseHex player, canvas]
      in
        if (isValidMove model.boardData model.mouseHex model.selectMouseHex) then
          group [drawRing model.mouseHex player, canvasWithMarker]
        else canvasWithMarker
  -- For now, other stages unimplemented
    _ -> Debug.todo "ADD FLOATING ELEMS FOR OTHER STATES!"

view : Model -> Html Msg
view model =
  let
    -- Add invisible border to prevent annoying edge glitch
    board = renderBoard edges_coords
    pieces = renderPieces model.boardData
    game = addFloatingElems model <| group [pieces, board] -- order important since we want pieces on top of board
    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
  in
    div (List.map (\(k, v) -> Attr.style k v) styles) [svg game]
