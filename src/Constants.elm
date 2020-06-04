module Constants exposing (..)

import Dict exposing (Dict)
import Collage exposing (Point)
import Color

-- Vertex state
-- Markers and rings are mutually exclusive in the internal board state
type Player = P1 | P2
type VState = Marker Player | Ring Player | None

-- Since coordinates of the internal state are all integers; we only need to
-- touch floats when we work with drawing
type alias IntPoint = (Int, Int)

-- Length of a side in pixels
side = 50
p1Color = Color.red
p2Color = Color.green
boardColor = Color.grey
borderColor = Color.black

-- Size of a ring radius in pixels
ring_size = 0.4 * side
marker_size = ring_size - 2

-- -30 degrees and +90 degrees for x and y axial unit vectors
radian_x = -1 * pi / 6
radian_y = pi / 2

-- x, y unit vectors in terms of pixels
unit_x : Point
unit_x = (side * cos radian_x, side * sin radian_x)
unit_y : Point
unit_y = (side * cos radian_y, side * sin radian_y)

-- Turns internal representation into cartesian coordinates, scaled appropriately
hex2pix : IntPoint -> Point
hex2pix (x,y) =
  let cart_x = (Tuple.first unit_x) * toFloat x + (Tuple.first unit_y) * toFloat y
      cart_y = (Tuple.second unit_x) * toFloat x + (Tuple.second unit_y) * toFloat y
  in
  (cart_x, cart_y)

-- Turns a cartesian pixel into the nearest hex coordinate
-- HARDCODED: assume y axis is vertical, so we don't have to solve system of eqs
pix2hex : Point -> IntPoint
pix2hex (cx, cy) =
  let x = cx / Tuple.first unit_x
      y = (cy - x * Tuple.second unit_x) / (Tuple.second unit_y)
  in
  (round x, round y)

vertices : List IntPoint
vertices = [(-5, -4), (-5, -3), (-5, -2), (-5, -1), (-4, -5), (-4, -4), (-4, -3), (-4, -2), (-4, -1), (-4, 0), (-4, 1), (-3, -5), (-3, -4), (-3, -3), (-3, -2), (-3, -1), (-3, 0), (-3, 1), (-3, 2), (-2, -5), (-2, -4), (-2, -3), (-2, -2), (-2, -1), (-2, 0), (-2, 1), (-2, 2), (-2, 3), (-1, -5), (-1, -4), (-1, -3), (-1, -2), (-1, -1), (-1, 0), (-1, 1), (-1, 2), (-1, 3), (-1, 4), (0, -4), (0, -3), (0, -2), (0, -1), (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (1, -4), (1, -3), (1, -2), (1, -1), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, -3), (2, -2), (2, -1), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (3, -2), (3, -1), (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (4, -1), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (5, 1), (5, 2), (5, 3), (5, 4)]

-- Initial empty board
emptyBoard : Dict IntPoint VState
emptyBoard =
  Dict.fromList (List.map (\v -> (v, None)) vertices)

edges : List (IntPoint, IntPoint)
edges = [((-5, -4), (-5, -3)), ((-5, -4), (-4, -4)), ((-5, -4), (-4, -3)), ((-5, -3), (-5, -2)), ((-5, -3), (-4, -3)), ((-5, -3), (-4, -2)), ((-5, -2), (-5, -1)), ((-5, -2), (-4, -2)), ((-5, -2), (-4, -1)), ((-5, -1), (-4, -1)), ((-5, -1), (-4, 0)), ((-4, -5), (-4, -4)), ((-4, -5), (-3, -5)), ((-4, -5), (-3, -4)), ((-4, -4), (-4, -3)), ((-4, -4), (-3, -4)), ((-4, -4), (-3, -3)), ((-4, -3), (-4, -2)), ((-4, -3), (-3, -3)), ((-4, -3), (-3, -2)), ((-4, -2), (-4, -1)), ((-4, -2), (-3, -2)), ((-4, -2), (-3, -1)), ((-4, -1), (-4, 0)), ((-4, -1), (-3, -1)), ((-4, -1), (-3, 0)), ((-4, 0), (-4, 1)), ((-4, 0), (-3, 0)), ((-4, 0), (-3, 1)), ((-4, 1), (-3, 1)), ((-4, 1), (-3, 2)), ((-3, -5), (-3, -4)), ((-3, -5), (-2, -5)), ((-3, -5), (-2, -4)), ((-3, -4), (-3, -3)), ((-3, -4), (-2, -4)), ((-3, -4), (-2, -3)), ((-3, -3), (-3, -2)), ((-3, -3), (-2, -3)), ((-3, -3), (-2, -2)), ((-3, -2), (-3, -1)), ((-3, -2), (-2, -2)), ((-3, -2), (-2, -1)), ((-3, -1), (-3, 0)), ((-3, -1), (-2, -1)), ((-3, -1), (-2, 0)), ((-3, 0), (-3, 1)), ((-3, 0), (-2, 0)), ((-3, 0), (-2, 1)), ((-3, 1), (-3, 2)), ((-3, 1), (-2, 1)), ((-3, 1), (-2, 2)), ((-3, 2), (-2, 2)), ((-3, 2), (-2, 3)), ((-2, -5), (-2, -4)), ((-2, -5), (-1, -5)), ((-2, -5), (-1, -4)), ((-2, -4), (-2, -3)), ((-2, -4), (-1, -4)), ((-2, -4), (-1, -3)), ((-2, -3), (-2, -2)), ((-2, -3), (-1, -3)), ((-2, -3), (-1, -2)), ((-2, -2), (-2, -1)), ((-2, -2), (-1, -2)), ((-2, -2), (-1, -1)), ((-2, -1), (-2, 0)), ((-2, -1), (-1, -1)), ((-2, -1), (-1, 0)), ((-2, 0), (-2, 1)), ((-2, 0), (-1, 0)), ((-2, 0), (-1, 1)), ((-2, 1), (-2, 2)), ((-2, 1), (-1, 1)), ((-2, 1), (-1, 2)), ((-2, 2), (-2, 3)), ((-2, 2), (-1, 2)), ((-2, 2), (-1, 3)), ((-2, 3), (-1, 3)), ((-2, 3), (-1, 4)), ((-1, -5), (-1, -4)), ((-1, -5), (0, -4)), ((-1, -4), (-1, -3)), ((-1, -4), (0, -4)), ((-1, -4), (0, -3)), ((-1, -3), (-1, -2)), ((-1, -3), (0, -3)), ((-1, -3), (0, -2)), ((-1, -2), (-1, -1)), ((-1, -2), (0, -2)), ((-1, -2), (0, -1)), ((-1, -1), (-1, 0)), ((-1, -1), (0, -1)), ((-1, -1), (0, 0)), ((-1, 0), (-1, 1)), ((-1, 0), (0, 0)), ((-1, 0), (0, 1)), ((-1, 1), (-1, 2)), ((-1, 1), (0, 1)), ((-1, 1), (0, 2)), ((-1, 2), (-1, 3)), ((-1, 2), (0, 2)), ((-1, 2), (0, 3)), ((-1, 3), (-1, 4)), ((-1, 3), (0, 3)), ((-1, 3), (0, 4)), ((-1, 4), (0, 4)), ((0, -4), (0, -3)), ((0, -4), (1, -4)), ((0, -4), (1, -3)), ((0, -3), (0, -2)), ((0, -3), (1, -3)), ((0, -3), (1, -2)), ((0, -2), (0, -1)), ((0, -2), (1, -2)), ((0, -2), (1, -1)), ((0, -1), (0, 0)), ((0, -1), (1, -1)), ((0, -1), (1, 0)), ((0, 0), (0, 1)), ((0, 0), (1, 0)), ((0, 0), (1, 1)), ((0, 1), (0, 2)), ((0, 1), (1, 1)), ((0, 1), (1, 2)), ((0, 2), (0, 3)), ((0, 2), (1, 2)), ((0, 2), (1, 3)), ((0, 3), (0, 4)), ((0, 3), (1, 3)), ((0, 3), (1, 4)), ((0, 4), (1, 4)), ((0, 4), (1, 5)), ((1, -4), (1, -3)), ((1, -4), (2, -3)), ((1, -3), (1, -2)), ((1, -3), (2, -3)), ((1, -3), (2, -2)), ((1, -2), (1, -1)), ((1, -2), (2, -2)), ((1, -2), (2, -1)), ((1, -1), (1, 0)), ((1, -1), (2, -1)), ((1, -1), (2, 0)), ((1, 0), (1, 1)), ((1, 0), (2, 0)), ((1, 0), (2, 1)), ((1, 1), (1, 2)), ((1, 1), (2, 1)), ((1, 1), (2, 2)), ((1, 2), (1, 3)), ((1, 2), (2, 2)), ((1, 2), (2, 3)), ((1, 3), (1, 4)), ((1, 3), (2, 3)), ((1, 3), (2, 4)), ((1, 4), (1, 5)), ((1, 4), (2, 4)), ((1, 4), (2, 5)), ((1, 5), (2, 5)), ((2, -3), (2, -2)), ((2, -3), (3, -2)), ((2, -2), (2, -1)), ((2, -2), (3, -2)), ((2, -2), (3, -1)), ((2, -1), (2, 0)), ((2, -1), (3, -1)), ((2, -1), (3, 0)), ((2, 0), (2, 1)), ((2, 0), (3, 0)), ((2, 0), (3, 1)), ((2, 1), (2, 2)), ((2, 1), (3, 1)), ((2, 1), (3, 2)), ((2, 2), (2, 3)), ((2, 2), (3, 2)), ((2, 2), (3, 3)), ((2, 3), (2, 4)), ((2, 3), (3, 3)), ((2, 3), (3, 4)), ((2, 4), (2, 5)), ((2, 4), (3, 4)), ((2, 4), (3, 5)), ((2, 5), (3, 5)), ((3, -2), (3, -1)), ((3, -2), (4, -1)), ((3, -1), (3, 0)), ((3, -1), (4, -1)), ((3, -1), (4, 0)), ((3, 0), (3, 1)), ((3, 0), (4, 0)), ((3, 0), (4, 1)), ((3, 1), (3, 2)), ((3, 1), (4, 1)), ((3, 1), (4, 2)), ((3, 2), (3, 3)), ((3, 2), (4, 2)), ((3, 2), (4, 3)), ((3, 3), (3, 4)), ((3, 3), (4, 3)), ((3, 3), (4, 4)), ((3, 4), (3, 5)), ((3, 4), (4, 4)), ((3, 4), (4, 5)), ((3, 5), (4, 5)), ((4, -1), (4, 0)), ((4, 0), (4, 1)), ((4, 0), (5, 1)), ((4, 1), (4, 2)), ((4, 1), (5, 1)), ((4, 1), (5, 2)), ((4, 2), (4, 3)), ((4, 2), (5, 2)), ((4, 2), (5, 3)), ((4, 3), (4, 4)), ((4, 3), (5, 3)), ((4, 3), (5, 4)), ((4, 4), (4, 5)), ((4, 4), (5, 4)), ((5, 1), (5, 2)), ((5, 2), (5, 3)), ((5, 3), (5, 4))]

-- Map edges into their cartesian coordinate representation
edges_coords : List (Point, Point)
edges_coords = List.map (\(p1, p2) -> (hex2pix p1, hex2pix p2)) edges
