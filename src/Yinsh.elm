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

import Constants exposing (VState(..), IntPoint, emptyBoard, vertices, edges, edges_coords, hex2pix, pix2hex,
                            ring_size, side)

-- Game state; add more to this as we think of more reasonable states
-- PlaceR -> Placing the rings initially 
-- SelectR -> selecting ring to move
-- Confirm -> choosing the move out of the possible ones
-- RemoveR -> remove a ring upon 5-in-row (or many if 2 5s are formed at once)
-- Win -> Display win message
-- True/False to toggle which side is moving
-- True - Red
-- False - Green
type GState = PlaceR Bool | SelectR Bool | Confirm Bool | RemoveR Bool Int | Win Bool 

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
  , mousePos : (Float, Float) -- Stored as COLLAGE COORDINATES
  , mouseHex : (Int, Int)     -- Hex coordinates of mouse, use this!
  }

type alias Flags =
  { windowWidth : Int
  , windowHeight : Int
  }

type Msg = WindowResize Int Int | 
           MouseMoved Point

initModel : Flags -> Model
initModel flags = {boardData = emptyBoard, 
            gameState = PlaceR True, 
            score = (0, 0),
            windowWidth = toFloat flags.windowWidth,
            windowHeight = toFloat flags.windowHeight,
            mousePos = (0, 0), -- stored in COLLAGE COORDINATES, automatically converteda
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
    E.onMouseMove decodeMouse
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    -- WindowResize has carries two ints next to it 
    WindowResize x y -> ({model | windowWidth = toFloat x, windowHeight = toFloat y}, Cmd.none)
    MouseMoved (x,y) -> 
      let (cx, cy) = xyToCollage model (x,y) 
      in ({model | mousePos = (cx, cy), mouseHex = pix2hex model.mousePos}, Cmd.none)
--------------------------
-- see https://github.com/elm/browser/blob/1.0.2/examples/src/Drag.elm for example of decoder interaction

-- Decoder for json -> Msg that contains x, y coords of mouse from JSON
decodeMouse : D.Decoder Msg
decodeMouse = 
  let decode_coords = D.map2 (\x y -> (x,y)) 
                      (D.field "clientX" D.float) (D.field "clientY" D.float)
  in 
    (D.map MouseMoved decode_coords)


-------------------------

-- Converts x,y from javascript to collage form 
-- Center at middle of canvas and then invert y-axis
xyToCollage : Model -> Point -> Point
xyToCollage model (mx, my) = (mx - model.windowWidth/2, model.windowHeight/2 - my)

-- Renders a single ring at the given hex coordinates 
drawRing : IntPoint -> Collage Msg
drawRing p = 
  let (cx, cy) = hex2pix p
  in (circle ring_size) |> 
      outlined (solid thick (uniform Color.black)) |> shift (cx, cy)

-- Checks if model currently have valid Hex coordinates
mouseHexValid : Model -> Bool
mouseHexValid model = Dict.member model.mouseHex model.boardData


renderBoard : List (Point, Point) -> Collage Msg 
renderBoard edges_coords =
  let edges = List.map (\(p1, p2) -> segment p1 p2 
              |> traced (solid thin (uniform Color.grey))) edges_coords 
              |> stack
      -- Need a border b/c of the glitch near the edges. Also looks better
      border = square (10 * side) |> outlined (solid thin (uniform Color.black))
  in
  group [border, edges]

view : Model -> Html Msg
view model =
  let
    -- Add invisible border to prevent annoying edge glitch
    board = renderBoard edges_coords
    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
  in
    if mouseHexValid model then
    let
      testRing = drawRing <| model.mouseHex
      drawShit = [testRing, board]
      canvas = svg <| group drawShit
    in 
      div (List.map (\(k, v) -> Attr.style k v) styles) [canvas]
    else 
      div (List.map (\(k, v) -> Attr.style k v) styles) [svg <| board]
