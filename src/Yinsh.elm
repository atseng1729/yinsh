module Yinsh exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Debug
import Time
import Collage exposing (segment, traced, solid, thin, uniform, Point)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Color

import Constants exposing (boardData, vertices, edges)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model = {}
type alias Flags = ()
type Msg = Tick Time.Posix

initModel : Model
initModel = {}

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

renderBoard : List (Point, Point) -> Html msg
renderBoard edges =
  List.map (\(p1, p2) -> segment p1 p2 |> traced (solid thin (uniform Color.grey))) edges
    |> stack
    |> svg

view : Model -> Html Msg
view model =
  let
    board = renderBoard edges
    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
  in
    div (List.map (\(k, v) -> Attr.style k v) styles) [ board ]
