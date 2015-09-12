module Game where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Window
import Text
import Array
import Random
import List exposing (..)
import Signal exposing ((<~), (~), sampleOn, foldp)
import Time exposing (..)

-- CONFIG
speed             = 500
boardSize         = 600
pillSize          = 15
playerSize        = pillSize
spawnInterval     = 57 / speed
interval          = every (second * spawnInterval)
delta             = fps 50
(bWidth, bHeight) = (boardSize, boardSize)
(hWidth, hHeight) = (bWidth / 2, bHeight / 2)

-- HELPER FUNCTIONS

vecAdd : Vec -> Vec -> Vec
vecAdd (px, py) (vx, vy) = (px + vx, py + vy)

vecSub : Vec -> Vec -> Vec
vecSub (px, py) (vx, vy) = (px - vx, py - vy)

vecLen : Vec -> Float
vecLen (x,y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS (x,y) t = (x * t, y * t)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (x, y) (ox, oy) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

-- INPUT

newPill : Float -> Color -> Pill
newPill x col = { defaultPill | color <- col
                              , pos   <- (x, hHeight) }

input : Signal (Float, (Int, Int))
input = (,) <~ Signal.map inSeconds delta
            ~ sampleOn delta
                (Signal.map2 relativeMouse Mouse.position (Signal.map center Window.dimensions))

randX : Signal Float -> Signal Float
randX sig =
  let
    coord t = seedFromTime t |> Random.generate (Random.float -hWidth hWidth)
                             |> fst
  in
    Signal.map coord sig

randColor : Signal Float -> Signal Color
randColor sig =
  let
    getColor i = if i == 1 then lightBlue else defaultPill.color
    color t = seedFromTime t |> Random.generate (Random.int 1 10)
                             |> fst
                             |> getColor
  in
    Signal.map color sig

seedFromTime : Time -> Random.Seed
seedFromTime t = Random.initialSeed <| round t

event : Signal Event
event =
  Signal.mergeMany [ Signal.map Tick input
                   , Signal.map2 (\x col -> Add <| newPill x col) (randX interval) (randColor interval)
                   , Signal.map (\_ -> Click) Mouse.clicks ]

-- MODEL

type alias Vec = (Float, Float)
type alias Pill = { pos   : Vec
                  , vel   : Vec
                  , rad   : Float
                  , color : Color }

type alias Game = { player  : Pill
                  , pills   : List Pill
                  , score   : Int
                  , state   : State }

type State = Start | Play | Over
type Event = Tick (Time, (Int, Int)) | Add Pill | Click

defaultPill = { pos   = (0, hHeight)
              , vel   = (0, -speed)
              , rad   = pillSize
              , color = lightRed }

defaultPlayer = { defaultPill | pos   <- (0,-hHeight - defaultPill.rad)
                              , rad   <- playerSize
                              , color <- black }

defaultGame = { player  = defaultPlayer
              , pills   = []
              , score   = 0
              , state   = Start }

-- UPDATE

stepPlay : Event -> Game -> Game
stepPlay event g =
  case event of
    Add p        -> { g | pills <- p :: g.pills }
    Click        -> g
    Tick (t, mp) ->
      let
        hit pill    = (vecLen <| vecSub g.player.pos pill.pos) < g.player.rad + pill.rad
        unculed     = filter (\{pos} -> snd pos >= negate hHeight) g.pills
        untouched   = filter (not << hit) unculed
        touched     = filter hit unculed
        hitColor c  = not <| isEmpty <| filter (\{color} -> color == c) touched
        hitBlue     = hitColor lightBlue
        hitRed      = hitColor lightRed
        out         = let (x,y) = mp in abs (toFloat x) > hWidth || abs (toFloat y) > hHeight
        g'          = { g | player  <- stepPlayer mp g.player
                          , pills   <- map (stepPill t) untouched
                          , score   <- if hitBlue then g.score + 1 else g.score }
      in
        if hitRed || out then { defaultGame | score <- g'.score
                                            , state <- Over } else g'

clickStart : Event -> Bool
clickStart event =
  case event of
    Click -> True
    _     -> False

stepGame : Event -> Game -> Game
stepGame event ({state} as g) =
  let playGame  = { defaultGame | state <- Play }
      waitClick = if clickStart event then playGame else g
  in
    case state of
      Play -> stepPlay event g
      _    -> waitClick

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos <| vecMulS p.vel t }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x,y) p = { p | pos <- (toFloat x, toFloat y) }

-- DISPLAY
tf : Float -> Float -> String -> Form
tf y scl str = Text.fromString str |> Text.color gray
                                   |> text
                                   |> scale scl
                                   |> move (0, y)

render : (Int, Int) -> Game -> Element
render (w, h) g =
  let formPill {rad, color, pos} = circle rad |> filled color
                                              |> move pos
      txts = case g.state of
              Start -> [ tf  70 6 "BluePiLL"
                       , tf -50 2 "Click to start" ]
              Play  -> [ tf   0 4 (toString g.score) ]
              Over  -> [ tf  70 6 "Game Over"
                       , tf   0 4 (toString g.score)
                       , tf -50 2 "Click to restart" ]
      forms = txts ++ (map formPill <| g.player :: g.pills)
  in
    color lightGray <| container w h middle
                    <| color white
                    <| collage bWidth bHeight forms

main =
  render
    <~ Window.dimensions
    ~ foldp stepGame defaultGame event