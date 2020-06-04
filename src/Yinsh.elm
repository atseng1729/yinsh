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
-- segment, traced, solid, thin, uniform, Point,
import Collage exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Color

import Constants exposing (..)
import Helper exposing (..)

-- Game state; add more to this as we think of more reasonable states
-- PlaceR -> Placing the rings initially
-- SelectR -> selecting ring to move
-- Confirm -> choosing the move out of the possible ones
-- RemoveR -> remove a ring upon 5-in-row (or many if 2 5s are formed at once)
-- Win -> Display win message
-- True/False to toggle which side is moving
-- True - Red
-- False - Green
type GState = PlaceR Int | SelectR Bool | Confirm Bool | RemoveR Bool Int | Win Bool

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
  }

type alias Flags =
  { windowWidth : Int
  , windowHeight : Int
  }

type Msg = WindowResize Int Int |
           MouseMoved IntPoint |
           MouseClick IntPoint

initModel : Flags -> Model
initModel flags = {boardData = emptyBoard,
            gameState = PlaceR 0,
            score = (0, 0),
            windowWidth = toFloat flags.windowWidth,
            windowHeight = toFloat flags.windowHeight,
            mouseHex = (0, 0)}

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
    -- WindowResize has carries two ints next to it
    WindowResize x y -> ({model | windowWidth = toFloat x, windowHeight = toFloat y}, Cmd.none)
    MouseMoved p ->
      ({model | mouseHex = p}, Cmd.none)
    MouseClick p ->
      if isValidHex model.boardData p then
        case model.gameState of
          PlaceR n ->
            let
              newVState = if isPlayer1 n then R_Ring else G_Ring
              newGState = if n < 9 then PlaceR (n+1) else SelectR True
              newBoardData = Dict.insert p newVState model.boardData
            in
              ({model | boardData = newBoardData, gameState = newGState}, Cmd.none)
          _ -> Debug.todo "Need to determine other cases for mouse click"
      else
        (model, Cmd.none)

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
drawRing : IntPoint -> Bool -> Collage Msg
drawRing p isP1 =
  let
    (cx, cy) = hex2pix p
    ringColor = if isP1 then p1Color else p2Color
  in
    (circle ring_size)
      |> outlined (solid thick (uniform ringColor))
      |> shift (cx, cy)

drawMarker : IntPoint -> Bool -> Collage Msg
drawMarker p isP1 =
  let
    (cx, cy) = hex2pix p
    markerColor = if isP1 then p1Color else p2Color
  in
    (circle marker_size)
      |> filled (uniform markerColor)
      |> shift (cx, cy)

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
    R_Marker -> drawMarker p True
    G_Marker -> drawMarker p False
    R_Ring   -> drawRing p True
    G_Ring   -> drawRing p False
    None     -> Debug.todo "renderPiece - should not reach here"

renderPieces : Dict IntPoint VState -> Collage Msg
renderPieces boardData =
  (Dict.toList boardData)
    |> List.filter (\(p, state) -> state /= None)
    |> List.map renderPiece
    |> group

view : Model -> Html Msg
view model =
  let
    -- Add invisible border to prevent annoying edge glitch
    board = renderBoard edges_coords
    pieces = renderPieces model.boardData
    game = group [pieces, board] -- order important since we want pieces on top of board
    -- TODO mouse over feature but i think i want to refactor gstate and make this a separate state
    -- if (isValidHex model.mouseHex) then
    --   mouseOverRing = drawRing model.mouseHex _____
    --   game = group [game, mouseOverRing]
    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
  in
    div (List.map (\(k, v) -> Attr.style k v) styles) [svg game]
