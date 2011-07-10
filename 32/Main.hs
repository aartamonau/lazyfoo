{-# LANGUAGE Arrows #-}

module Main ( main )
       where

import Debug.Trace

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

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode(ExitSuccess))


import qualified Timer
import SDLUtils (loadImage, setColorKey)

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , closed  :: Bool
        }

data Dot = Dot Double Double

data Context =
  Context { c_screen  :: SDL.Surface
          , c_bgColor :: SDL.Pixel
          , c_dot     :: SDL.Surface }

screenWidth, screenHeight :: Num a => a
screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

dotD, dotR :: Double
dotD            = 20
dotR            = dotD / 2

-- dot's position at the beginning
dotX, dotY :: Double
dotX            = dotR
dotY            = dotR

dotVelocity :: Double
dotVelocity     = 200

framesPerSecond = 200
ticksPerFrame   = 1 / framesPerSecond

reactimate' init sense actuate sf =
  reactimate init (const sense) (const actuate) sf

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen   <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  white    <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff
  dotImage <- (`setColorKey` (0xff, 0xff, 0xff)) =<< loadImage "dot.bmp"

  let context = Context screen white dotImage

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

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.stop timer
        Timer.start timer

        if isNothing input
          then return (time, Just defaultInput)
          else return (time, input)

  let actuate Nothing    = return True
      actuate (Just dot) = renderDot context dot >> return False

  reactimate' init sense actuate dot

  quit

  where quit   = SDL.quit >> exitWith ExitSuccess
        arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        defaultInput = Input NoEvent NoEvent False

dot :: SF Input (Maybe Dot)
dot =
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

    x <- loopPre' dotX (position >>> bound 0 screenWidth) -< (vx, dt)
    y <- loopPre' dotY (position >>> bound 0 screenHeight) -< (vy, dt)

    returnA -< if closed then Nothing else Just (Dot x y)

  where v :: SF (Event a, Event b) Double
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dotVelocity
                        `rMerge`
                        deactivate `tag` 0

        bound :: Double -> Double -> SF Double Double
        bound l u =
          proc p -> do
            let p' | p > u'    = u'
                   | p < l'    = l'
                   | otherwise = p
            returnA -< p'
          where u' = u - dotR
                l' = l + dotR

        loopPre' :: b -> SF (a, b) b -> SF a b
        loopPre' v sf = loopPre v (sf >>> double)
          where double :: Arrow arr => arr a (a, a)
                double = arr id &&& arr id

        position :: SF ((Double, Double), Double) Double
        position =
          proc ((v, dt), p) ->
            returnA -< p + v * dt

renderDot :: Context -> Dot -> IO ()
renderDot (Context screen white dot) (Dot x y) = do
  fillBg
  drawDot (round x) (round y)
  SDL.flip screen

  where fillBg      = SDL.fillRect screen Nothing white
        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - (round dotR))
                                                  (y - (round dotR)) 0 0)
