{-# LANGUAGE Arrows, PatternGuards, ViewPatterns #-}

module Main (main)
       where

import Control.Arrow (returnA, (&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import qualified Data.Map as M
import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  filterE, tag, repeatedly,
                  rMerge, hold, dAccumHoldBy)
import qualified Graphics.UI.SDL as SDL

import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

levelWidth      = 1280
levelHeight     = 960

framesPerSecond = 30
ticksPerFrame   = 1 / framesPerSecond

data Dot = Dot Int Int

dotD            = 20
dotR            = dotD `div` 2

-- starting position of the dot controlled by user
dotX = 10
dotY = 10

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , closed  :: Bool
        }

data GContext =
  GContext { c_screen :: SDL.Surface
           , c_bg     :: SDL.Surface
           , c_dot    :: SDL.Surface }

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  bg     <- loadImage "bg.png"
  dot    <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "dot.bmp"

  SDL.setCaption "Move the Dot" ""

  return $ GContext screen bg dot

quitSDL :: IO ()
quitSDL = SDL.quit

renderScreen :: GContext -> SDL.Rect -> Dot -> IO ()
renderScreen (GContext screen bg dot) camera (Dot x y) = do
  fillBg
  drawDot x' y'
  SDL.flip screen

  where fillBg      = SDL.blitSurface bg (Just camera) screen Nothing
        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dotR) (y - dotR) 0 0)

        x' = x - SDL.rectX camera
        y' = y - SDL.rectY camera

main :: IO ()
main = do
  gcontext <- initSDL

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

  let actuate Nothing              _       = return True
      actuate _                    NoEvent = return False
      actuate (Just dot@(Dot x y)) _       = do
        renderScreen gcontext (SDL.Rect cx cy screenWidth screenHeight) dot
        return False

        where cx | left < 0           = 0
                 | right > levelWidth = levelWidth - screenWidth
                 | otherwise          = left
                where left  = x - screenWidth `div` 2
                      right = left + screenWidth

              cy | top < 0              = 0
                 | bottom > levelHeight = levelHeight - screenHeight
                 | otherwise            = top
                where top    = y - screenHeight `div` 2
                      bottom = top + screenHeight

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (dot beat &&& beat)

  quitSDL

  where arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        defaultInput = Input NoEvent NoEvent False

dot :: SF () (Event ()) -> SF Input (Maybe Dot)
dot beat =
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

    let vx = vr - vl
    let vy = vd - vu

    beat' <- beat -< ()

    rec
      (rvx, rvy) <- respectBoundaries -< ((x, y), (vx, vy))

      x <- dAccumHoldBy (+) dotX -< beat' `tag` rvx
      y <- dAccumHoldBy (+) dotY -< beat' `tag` rvy

    returnA -< if closed then Nothing else Just (Dot x y)

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dv
                        `rMerge`
                        deactivate `tag` 0

        dv = dotR

        respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
        respectBoundaries =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if breaksBoundaries (x + vx, y) then 0 else vx
            let rvy = if breaksBoundaries (x, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        breaksBoundaries :: (Int, Int) -> Bool
        breaksBoundaries (x, y) =
          or [left < 0, top < 0, bottom > levelHeight, right > levelWidth]
          where left   = x - dotR
                right  = x + dotR
                top    = y - dotR
                bottom = y + dotR
