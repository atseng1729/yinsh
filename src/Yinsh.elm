-- For debugging purposes, exposing everything
module Yinsh exposing (..)

import Browser
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

import Constants exposing (VState, IntPoint, emptyBoard, vertices, edges, edges_coords, hex2pix, pix2hex,
                            ring_size)

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
  }

type alias Flags = ()
type Msg = Tick Time.Posix

initModel : Model
initModel = {boardData = emptyBoard, 
            gameState = PlaceR True, 
            score = (0, 0)}

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

-- Renders a single ring at the given hex coordinates
drawRing : IntPoint -> Collage Msg
drawRing p = 
  let (cx, cy) = hex2pix p
  in (circle ring_size) |> 
      outlined (solid thick (uniform Color.black)) |> shift (cx, cy)


renderBoard : List (Point, Point) -> Collage Msg 
renderBoard edges_coords =
  List.map (\(p1, p2) -> segment p1 p2 |> traced (solid thin (uniform Color.grey))) edges_coords
    |> stack

view : Model -> Html Msg
view model =
  let
    board = renderBoard edges_coords
    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
    testRing = drawRing (2, 2)
    canvas = svg <| group [testRing, board]
  in
    div (List.map (\(k, v) -> Attr.style k v) styles) [canvas]
