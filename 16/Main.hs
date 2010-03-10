{-# LANGUAGE Arrows #-}

module Main -- ( main )
       where

import Debug.Trace


import Control.Applicative (pure, (<*>), (<$>))
import Control.Arrow (Arrow, (***), (&&&), (>>>), returnA, arr)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)

import Data.Either (either)
import Data.Function (fix)
import qualified Data.Map as M
import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  isEvent,
                  tag, lMerge, rMerge, mergeBy, hold,
                  dAccumHoldBy, repeatedly,
                  constant, reactimate, filterE)

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode(ExitSuccess))


import qualified Timer
import SDLUtils (loadImage, setColorKey)
import Utils (takeWhileM)

instance Show a => Show (Event a) where
  show (Event a) = "Event " ++ show a
  show NoEvent   = "NoEvent"

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , closed  :: Bool
        }
  deriving Show

data Dot = Dot Int Int
         deriving Show

data Context =
  Context { c_screen  :: SDL.Surface
          , c_bgColor :: SDL.Pixel
          , c_dot     :: SDL.Surface }

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

dotWidth        = 20
dotHeight       = 20

framesPerSecond = 30
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
        fix $ \loop -> do
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

          time <-((/1000) . fromIntegral) <$> Timer.getTicks timer
          Timer.stop timer
          Timer.start timer

          if isNothing input
            then return (time, Just defaultInput)
            else return (time, input)

  let actuate Nothing _  = return True
      actuate (Just dot) beat
        | isEvent beat = renderDot context dot >> return False
        | otherwise    = return False

  let beat = repeatedly ticksPerFrame ()
  reactimate' init sense (uncurry actuate) (velocity beat &&& beat)

  quit

  where quit   = SDL.quit >> exitWith ExitSuccess
        arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        defaultInput = Input NoEvent NoEvent False

velocity :: SF () (Event ()) -> SF Input (Maybe Dot)
velocity beat =
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

    beat' <- beat -< ()

    rec
      bvl <- bound 0 (>)            -< (x, vl)
      bvr <- bound screenWidth (<)  -< (x, vr)
      bvu <- bound 0 (>)            -< (y, vu)
      bvd <- bound screenHeight (<) -< (y, vd)

      let vx = bvr - bvl
      let vy = bvd - bvu

      x <- dAccumHoldBy (+) 0 -< beat' `tag` vx
      y <- dAccumHoldBy (+) 0 -< beat' `tag` vy

    returnA -< if closed then Nothing else Just (Dot x y)

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` 10
                        `rMerge`
                        deactivate `tag` 0

        bound :: Int -> (Int -> Int -> Bool) -> SF (Int, Int) Int
        bound b pf =
          proc (p, v) ->
            returnA -< if pf p b then v else 0

renderDot :: Context -> Dot -> IO ()
renderDot (Context screen white dot) (Dot x y) = do
  fillBg
  drawDot x y
  SDL.flip screen

  where fillBg         = SDL.fillRect screen Nothing white
        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dx) (y - dy) 0 0)
        dx             = dotWidth `div` 2
        dy             = dotHeight `div` 2
