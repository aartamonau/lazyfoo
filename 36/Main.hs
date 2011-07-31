{-# Language Arrows #-}

module Main ( main )
       where

import Control.Arrow (Arrow, (&&&), (>>>), arr, returnA)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import qualified Data.Map as M
import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF, (-->),
                  isEvent, iPre,
                  tag, rMerge, hold,
                  dHold, repeatedly,
                  reactimate, filterE, time, loopPre)

import GHC.Float (double2Float)


import Graphics.Rendering.OpenGL ( ($=) )
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode(ExitSuccess))


import qualified Timer

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , closed  :: Bool
        }

screenWidth, screenHeight :: Num a => a
screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

reactimate' init sense actuate sf =
  reactimate init (const sense) (const actuate) sf

initGL :: IO ()
initGL = do
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 screenWidth screenHeight 0 (-1) 1

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

-- square's position at the beginning
squareX, squareY :: Float
squareX = 0
squareY = 0

squareSide :: Float
squareSide = 20

squareVelocity :: Float
squareVelocity = 600

data Square = Square !Float !Float

renderSquare :: Square -> IO ()
renderSquare (Square x y) = do
  GL.translate $ GL.Vector3 x y 0

  GL.renderPrimitive GL.Quads $ do
    GL.color $ GL.Color4 (1 :: GL.GLfloat) 1 1 1

    GL.vertex $ GL.Vertex3 (0 :: GL.GLfloat) 0 0
    GL.vertex $ GL.Vertex3 squareSide 0 0
    GL.vertex $ GL.Vertex3 squareSide squareSide 0
    GL.vertex $ GL.Vertex3 0 squareSide 0

  GL.loadIdentity

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen   <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.OpenGL]

  initGL

  SDL.setCaption "Move the Dot" ""

  timer <- Timer.mkTimer

  let init  = Timer.start timer >> return (Input NoEvent NoEvent False)

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.Quit           -> Just $ Input NoEvent NoEvent True
                SDL.KeyDown keysym ->
                  case handleKeysym keysym of
                    Just arrow -> Just $ Input (Event arrow) NoEvent False
                    _          -> Nothing
                SDL.KeyUp keysym ->
                  case handleKeysym keysym of
                    Just arrow -> Just $ Input NoEvent (Event arrow) False
                    _          -> Nothing
                _                  -> Nothing

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.stop timer
        Timer.start timer

        if isNothing input
          then return (time, Just defaultInput)
          else return (time, input)

  let actuate Nothing       = return True
      actuate (Just square) = do
        GL.clear [GL.ColorBuffer]
        renderSquare square
        SDL.glSwapBuffers
        return False

  reactimate' init sense actuate square

  quit

  where quit   = SDL.quit >> exitWith ExitSuccess
        arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        defaultInput = Input NoEvent NoEvent False

square :: SF Input (Maybe Square)
square =
  proc input@(Input down up closed) -> do
    let leftD  = filterE (== LeftKey) down
    let leftU  = filterE (== LeftKey) up
    let rightD = filterE (== RightKey) down
    let rightU = filterE (== RightKey) up

    let upD   = filterE (== UpKey) down
    let upU   = filterE (== UpKey) up
    let downD = filterE (== DownKey) down
    let downU = filterE (== DownKey) up

    vl <- v -< (leftD, leftU)
    vr <- v -< (rightD, rightU)
    vu <- v -< (upD, upU)
    vd <- v -< (downD, downU)

    ct <- time -< ()
    pt <- time >>> iPre 0 -< ()
    let dt = ct - pt

    let vx = vr - vl
    let vy = vd - vu

    x <- loopPre' squareX (position >>> bound 0 screenWidth) -< (vx, dt)
    y <- loopPre' squareY (position >>> bound 0 screenHeight) -< (vy, dt)

    returnA -< if closed then Nothing else Just (Square x y)

  where v :: SF (Event a, Event b) Float
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` squareVelocity
                        `rMerge`
                        deactivate `tag` 0

        bound :: Float -> Float -> SF Float Float
        bound l u =
          proc p -> do
            let p' | p > u'    = u'
                   | p < l'    = l'
                   | otherwise = p
            returnA -< p'
          where u' = u - squareSide
                l' = l

        loopPre' :: b -> SF (a, b) b -> SF a b
        loopPre' v sf = loopPre v (sf >>> double)
          where double :: Arrow arr => arr a (a, a)
                double = arr id &&& arr id

        position :: SF ((Float, Double), Float) Float
        position =
          proc ((v, dt), p) ->
            returnA -< p + v * (double2Float dt)
