module Game where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Window
import Array
import Random
import List exposing (..)
import Signal exposing ((<~), (~), sampleOn, foldp)
import Time exposing (..)

(bWidth, bHeight) = (400, 400)
(hWidth, hHeight) = (bWidth / 2, bHeight / 2)
delta = fps 60
interval = every (second * 2)

type alias Vec = (Float, Float)
type alias Pill = { pos : Vec
                  , vel : Vec
                  , rad : Float
                  , color : Color }

type alias Game = { player : Pill
                  , pills : List Pill }

type Event = Tick (Time, (Int, Int)) | Add Pill

defaultPill = { pos = (0, hHeight)
              , vel = (0, -150)
              , rad = 15
              , color = lightRed }

defaultPlayer = { defaultPill | pos <- (0,0)
                                , color <- black }

defaultGame = { player = defaultPlayer
              , pills = [] }

newPill : Float -> Color -> Pill
newPill x col = { defaultPill | color <- col
                              , pos <- (x, hHeight) }

stepGame : Event -> Game -> Game
stepGame event ({player, pills} as g) =
  case event of
    Tick (t, mp) ->  let
                        hit pill = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
                        unculed = filter (\{pos} -> snd pos >= negate hHeight) pills
                        untouched = filter (not << hit) unculed
                      in
                        { g | player <- stepPlayer mp player
                            , pills <- map (stepPill t) untouched }
    Add p        -> { g | pills <- p :: g.pills }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos
                            <| vecMulS p.vel t }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x,y) p = { p | pos <- (toFloat x, toFloat y) }

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

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let formPill {rad, color, pos} = circle rad |> filled color
                                              |> move pos
      forms = formPill game.player :: map formPill game.pills
  in
    color lightGray
    <| container w h middle
    <| color white
    <| collage bWidth bHeight forms


input = (,)
        <~ Signal.map inSeconds delta
        ~ sampleOn delta
            (Signal.map2 relativeMouse
              Mouse.position (Signal.map center Window.dimensions))

randX : Signal Float -> Signal Float
randX sig =
  let coord t = fstseedFromTime t |> Random.float -hWidth hWidth |> Random.generate
  in Signal.map coord sig

--randColor : Signal Float -> Signal Color
randColor sig =
  let chooseColor i = if i == 1 then lightBlue else defaultPill.color
      color t = seedFromTime t |> Random.int 1 10 |> Random.generate |> fst |> chooseColor
  in Signal.map color sig

seedFromTime : Time -> Random.Seed
seedFromTime t = Random.initialSeed <| round t


event = Signal.merge (Signal.map Tick input)
                     (Signal.map2 (\x col -> Add <| newPill x col) (randX interval) (randColor interval))

main =
  render
    <~ Window.dimensions
    ~ foldp stepGame defaultGame event
--}