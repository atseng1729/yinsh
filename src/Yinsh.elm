-- For debugging purposes, exposing everything
module Yinsh exposing (main)

import Browser
import Browser.Events as E

import Json.Decode as D

import Html exposing (..)
import Html.Events as Events
import Html.Attributes as Attr
import Debug
import Time
import Dict exposing (Dict)

import Collage exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Collage.Events as CE

import Constants exposing (..)
import Helper exposing (..)

-- Game state; add more to this as we think of more reasonable states
-- PlaceR -> Placing the rings initially
-- SelectR -> selecting ring to move
-- Confirm -> choosing the move out of the possible ones
-- RemoveM -> Choose markers to remove
-- RemoveR -> remove a ring upon 5-in-row (or many if 2 5s are formed at once, have to figure out how to select row for these edge cases...)
-- Win -> Display win message
-- True/False to toggle which side is moving
-- Colors: R, G
type GState = PlaceR Player | SelectR Player | Confirm Player | RemoveM Player Player | RemoveR Player Player Int | Win Player

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
  , mouseHex : IntPoint     -- Hex coordinates of mouse, use this!
  , selectMouseHex : IntPoint -- Hex coordinates of mouse in last state, used for confirm + other stuff maybe in future
  , p1Rings : Int          -- Current Number of Red Rings on Board
  , p2Rings : Int
  , validMoves : List IntPoint -- List of valid coordinates that can be moved to; only used currently for drawing possible moves
  , possibleRemoveMarkers : List IntPoint
  , toBeRemovedMarkers : List IntPoint
  }

type alias Flags =
  {
  }

type Msg = MouseMoved IntPoint |
           MouseClick IntPoint

emptyModel =
  { boardData = emptyBoard
  , gameState = PlaceR P1
  , score = (0, 0)
  , mouseHex = (0, 0)
  , selectMouseHex = (0, 0)
  , p1Rings = 0
  , p2Rings = 0
  , validMoves = []
  , possibleRemoveMarkers = []
  , toBeRemovedMarkers = []
  }

-- Move the central green ring to generate 2 red rows, one of which is a 7 in a row and the other a 6
debugModel = { boardData = Dict.fromList [((-5,-4),Ring P2),((-5,-3),None),((-5,-2),None),((-5,-1),None),((-4,-5),Ring P1),((-4,-4),Marker P2),((-4,-3),Marker P2),((-4,-2),None),((-4,-1),None),((-4,0),None),((-4,1),Ring P2),((-3,-5),Ring P2),((-3,-4),Ring P1),((-3,-3),Marker P1),((-3,-2),None),((-3,-1),None),((-3,0),None),((-3,1),Marker P1),((-3,2),None),((-2,-5),Ring P2),((-2,-4),Marker P1),((-2,-3),Ring P1),((-2,-2),Marker P1),((-2,-1),None),((-2,0),None),((-2,1),Marker P1),((-2,2),Marker P1),((-2,3),None),((-1,-5),Ring P1),((-1,-4),Marker P2),((-1,-3),None),((-1,-2),None),((-1,-1),None),((-1,0),None),((-1,1),Marker P1),((-1,2),Marker P1),((-1,3),None),((-1,4),None),((0,-4),None),((0,-3),Marker P2),((0,-2),None),((0,-1),None),((0,0),Ring P2),((0,1),Marker P1),((0,2),Marker P1),((0,3),None),((0,4),None),((1,-4),None),((1,-3),None),((1,-2),None),((1,-1),None),((1,0),Marker P1),((1,1),Marker P2),((1,2),Marker P1),((1,3),None),((1,4),None),((1,5),None),((2,-3),None),((2,-2),None),((2,-1),None),((2,0),Marker P1),((2,1),Marker P1),((2,2),Marker P2),((2,3),None),((2,4),None),((2,5),None),((3,-2),None),((3,-1),None),((3,0),Marker P2),((3,1),Marker P2),((3,2),Marker P1),((3,3),Marker P2),((3,4),None),((3,5),None),((4,-1),None),((4,0),Ring P1),((4,1),Marker P1),((4,2),Marker P1),((4,3),Marker P1),((4,4),None),((4,5),None),((5,1),None),((5,2),None),((5,3),None),((5,4),None)], gameState = SelectR P2, mouseHex = (4,0), p1Rings = 5, p2Rings = 5, possibleRemoveMarkers = [], score = (0,0), selectMouseHex = (4,1), toBeRemovedMarkers = [], validMoves = [(2,-1),(4,-1),(4,0),(4,4),(5,1),(5,2)] }

initModel : Flags -> Model
initModel flags = emptyModel

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
    -- map : (a -> value) -> Decoder a -> Decoder value. remove later but helpful now
    -- E.onMouseMove (decodeMouse model False),
    -- E.onMouseDown (decodeMouse model True)
  ]

processRowWrapper : Model -> Dict IntPoint VState -> Player -> Player -> (Int, Int) -> (Model, Cmd Msg)
processRowWrapper model newBoardData player prevPlayer newScore =
  let
    otherPlayer = otherP player
  in
    case checkAllRows newBoardData player of
      Just (0, points) -> -- Multiple non-independent rings
        ({model | gameState = RemoveM player prevPlayer, 
                  boardData = newBoardData, possibleRemoveMarkers = points, 
                  score = newScore, toBeRemovedMarkers = []}, 
                  Cmd.none)
      Just (newNumRings, points) ->
        ({model | gameState = RemoveR player prevPlayer newNumRings, 
                  boardData = newBoardData, toBeRemovedMarkers = points, 
                  score = newScore}, Cmd.none)
      Nothing ->
        case checkAllRows newBoardData otherPlayer of
          Just (0, points) ->
            ({model | gameState = RemoveM otherPlayer prevPlayer, 
                      boardData = newBoardData, possibleRemoveMarkers = points, 
                      score = newScore, toBeRemovedMarkers = []}
                      ,Cmd.none)
          Just (otherPNumRings, points) ->
            ({model | gameState = RemoveR otherPlayer prevPlayer otherPNumRings, 
                      boardData = newBoardData, toBeRemovedMarkers = points, score = newScore}
                      ,Cmd.none)
          Nothing ->
            ({model | gameState = SelectR (otherP prevPlayer), boardData = newBoardData, score = newScore}
                      ,Cmd.none)

-- Big master function basically translates the rules manual of Yinsh 
-- into code. Many of the checks are to handle edge cases and for
-- smooth UI interaction.

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMoved pt ->
      ({model | mouseHex = pt}, Cmd.none)

    MouseClick pt ->
      case model.gameState of
        PlaceR player ->
          if isEmptyHex model.boardData pt then
            let
              other = otherP player
              -- So that when there are 9 rings on the board, 
              -- the appropriate state change will be triggered after placing the 10th
              newGState = if (model.p1Rings + model.p2Rings) < 9 then PlaceR other else SelectR other
              newBoardData = Dict.insert pt (Ring player) model.boardData
              newModel = {model | boardData = newBoardData, gameState = newGState}
            in
              (addRing newModel player, Cmd.none)
          else
            (model, Cmd.none)
        SelectR player ->
          if isPlayerRing model.boardData pt player then -- Process the new valid moves for display in confirm
            let newValidMoves = getValidMoves model.boardData pt
            in ({model | gameState = Confirm player, 
                selectMouseHex = model.mouseHex, validMoves = newValidMoves},
                Cmd.none)
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
              processRowWrapper model newBoardData player player model.score
          else -- if click on invalid point, then transition back to select phase
            ({model | gameState = SelectR player, selectMouseHex = model.mouseHex},
              Cmd.none)

        RemoveM player prevPlayer ->
          if (List.member pt model.possibleRemoveMarkers) && not (List.member pt model.toBeRemovedMarkers) then
            let
              newRemovedMarkers = pt::model.toBeRemovedMarkers
              fiveSelected = 5 == List.length newRemovedMarkers
            in
              if not fiveSelected then
                ({model | toBeRemovedMarkers = newRemovedMarkers}, Cmd.none)
              else if isValidRow newRemovedMarkers then
                ({model | gameState = RemoveR player prevPlayer 1, toBeRemovedMarkers = newRemovedMarkers}, Cmd.none)
              else
                ({model | toBeRemovedMarkers = []}, Cmd.none)
          else
            (model, Cmd.none)

        RemoveR player prevPlayer numRings ->
          if isPlayerRing model.boardData pt player then
            if numRings == 1 then
              let
                otherPlayer = otherP player
                newBoardData = removeMarkers model.boardData model.toBeRemovedMarkers
                  |> Dict.insert pt None
                newScore = updateScore model.score player
              in
                if isWinner newScore then
                  ({model | gameState = Win player, boardData = newBoardData, score = newScore}, Cmd.none)
                else -- check if opponent had any rows formed
                  processRowWrapper model newBoardData player prevPlayer newScore
            else
              let
                newScore = updateScore model.score player
                newGState = if isWinner newScore then Win player 
                            else RemoveR player prevPlayer (numRings - 1)
                newBoardData = model.boardData
                  |> Dict.insert pt None
              in
                ({model | gameState = newGState, boardData = newBoardData, score = newScore}, Cmd.none)
          else
            (model, Cmd.none)
        Win player ->
          (model, Cmd.none)

--------------------------
-- see https://github.com/elm/browser/blob/1.0.2/examples/src/Drag.elm for example of decoder interaction
-- Decoder for json -> Msg that contains x, y coords of mouse from JSON

decodeMouse : Bool -> D.Decoder Msg
decodeMouse isClick =
  let
    decode_coords = D.map2 (\x y -> (x,y)) (D.field "offsetX" D.float) (D.field "offsetY" D.float)
    p = D.map (mouseInputToHex) decode_coords
    msg_type = if isClick then MouseClick else MouseMoved
  in
    (D.map msg_type p)

--------------------------
-- Converts x,y from javascript to IntPoint
-- Center at middle of canvas and then invert y-axis
mouseInputToHex : Point -> IntPoint
mouseInputToHex (mx, my) =
  (mx - borderLength/2, borderLength/2 - my) |> pix2hex

-- Extract current player; useful for visualizing whose turn it is via thicker rings
getPlayer : GState -> Player
getPlayer state = case state of
  PlaceR p -> p
  SelectR p -> p 
  Confirm p -> p 
  RemoveM p1 p2 -> p1
  RemoveR p1 p2 n -> p1
  Win p -> p

-- Renders a single ring at the given hex coordinates
-- Makes the rings slightly thicker for the player that should move!
drawRing : IntPoint -> Player -> Collage Msg
drawRing p player =
  let
    ringColor = if (player == P1) then p1Color else p2Color
  in
    circle ring_size
      |> outlined (solid thick (uniform ringColor))
      |> shift (hex2pix p)

blackenRing : IntPoint -> Collage Msg
blackenRing p =
  circle ring_size
    |> outlined (solid thick (uniform blackenColor))
    |> shift (hex2pix p)

-- Renders the rings in the score; starts from the corner (-5 * side)
-- And moves by multiples of 2.2 ring_radiuses instead of 2 for better aesthetic appeal
scoreRing : Player -> Int -> Bool -> Collage Msg
scoreRing player num colored =
  let
    floatNum = toFloat num
    ringColor = if not colored then boardColor else if player == P1 then p1Color else p2Color
    position = if player == P1 then
                 (-5 * side + (7.8 - 2.2 * floatNum) * ring_size, -5 * side + 1.2 * ring_size)
               else
                 (5 * side - (7.8 - 2.2 * floatNum) * ring_size, 5 * side - 1.2 * ring_size)
  in
    circle ring_size
      |> outlined (solid thick (uniform ringColor))
      |> shift position

drawMarker : IntPoint -> Player -> Collage Msg
drawMarker p player =
  let
    markerColor = if (player == P1) then p1Color else p2Color
  in
    circle marker_size
      |> filled (uniform markerColor)
      |> shift (hex2pix p)

-- drawDot; just draw a smaller black dot
drawDot : IntPoint -> Collage Msg
drawDot p =
  circle dot_size
    |> filled (uniform dotColor)
    |> shift (hex2pix p)

-- Draw ring and marker in the upper left corner corresponding to corresponding player's turn
drawTurnRing : GState -> Collage Msg 
drawTurnRing g = 
  let color = case getPlayer g of 
                P1 -> p1Color
                P2 -> p2Color 
  in 
    circle (4.6*side)
    |> outlined (solid thin (uniform color))

highlightMarker : IntPoint -> Collage Msg
highlightMarker p =
  circle marker_size
    |> outlined (solid thick (uniform highlightColor))
    |> shift (hex2pix p)

blackenMarker : IntPoint -> Collage Msg
blackenMarker p =
  circle marker_size
    |> filled (uniform blackenColor)
    |> shift (hex2pix p)

renderScore : (Int, Int) -> Collage Msg
renderScore (x, y) =
  [ scoreRing P1 1 (x >= 1)
  , scoreRing P1 2 (x >= 2)
  , scoreRing P1 3 (x >= 3)
  , scoreRing P2 1 (y >= 1)
  , scoreRing P2 2 (y >= 2)
  , scoreRing P2 3 (y >= 3)
  ] |> group

renderBoard : Player -> List (Point, Point) -> Collage Msg
renderBoard p edges_coords =-- TODO: change name to isEmptyHex
  let edges = List.map (\(p1, p2) -> segment p1 p2
              |> traced (solid thin (uniform boardColor))) edges_coords
              |> group
      -- Need a border b/c of the glitch near the edges. Also looks better
      playerTurnColor = if p == P1 then p1Color else p2Color
      border = square (10 * side) |> outlined (solid thin (uniform playerTurnColor))
  in
  group [border, edges]

declareWinner : Player -> Html Msg
declareWinner player =
  case player of
    P1 -> h3 [] [text "Player 1 won!!"]
    P2 -> h3 [] [text "Player 2 won!!"]

renderPiece : (IntPoint, VState) -> Collage Msg
renderPiece (p, state) =
  case state of
    Marker player -> drawMarker p player
    Ring player -> drawRing p player
    None     -> Debug.todo "renderPiece - should not reach here"

-- Used to add extra dots for valid move markers
renderDots : List (IntPoint) -> Collage Msg
renderDots ps =
  group <| List.map drawDot ps

-- Used to highlight markers during marker selection for removal (>5 markers case)
renderHighlighting : List (IntPoint) -> Collage Msg
renderHighlighting ps =
  group <| List.map highlightMarker ps

-- Renders already selected markers during RemoveM state
renderBlackenMarkers : List (IntPoint) -> Collage Msg
renderBlackenMarkers ps =
  group <| List.map blackenMarker ps

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
    RemoveM player _ ->
      if List.member model.mouseHex model.possibleRemoveMarkers then
        group [renderHighlighting model.possibleRemoveMarkers, 
              renderBlackenMarkers (model.mouseHex :: model.toBeRemovedMarkers), 
              canvas]
      else
        group [renderHighlighting model.possibleRemoveMarkers, 
              renderBlackenMarkers model.toBeRemovedMarkers, canvas]
    RemoveR player _ _ ->
      if isPlayerRing model.boardData model.mouseHex player then
        group [blackenRing model.mouseHex, 
              renderBlackenMarkers model.toBeRemovedMarkers, canvas]
      else
        group [renderBlackenMarkers model.toBeRemovedMarkers, canvas]
    Win player ->
      canvas

view : Model -> Html Msg
view model =
  let
    -- Add invisible border to prevent annoying edge glitch
    board = renderBoard (getPlayer model.gameState) edges_coords
    score = renderScore model.score
    pieces = renderPieces model.boardData
    -- turnRing = drawTurnRing model.gameState
    game = addFloatingElems model <| group [pieces, score, board] -- order important since we want pieces on top of board

    styles =
      [ ("position", "fixed")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translate(-50%, -50%)")
      ]
    -- Encoded into List (Attribute Msg) format
    attr_styles = (List.map (\(k, v) -> Attr.style k v) styles)

    -- Mouse listeners; only attach to the game
    mListeners : List (Attribute Msg)
    mListeners =
      [
          Events.on "mousemove" (decodeMouse False)
        , Events.on "mousedown" (decodeMouse True)
      ]
  in
    case model.gameState of
      Win player -> div attr_styles [svg game, declareWinner player]
      _          -> div (attr_styles ++ mListeners) [svg game]
