{-# LANGUAGE TupleSections #-}

module Main ( main )
       where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (forM_, when)

import Data.Function (fix)

import FRP.Reactive (Event, Behavior,
                     flipFlop, sumB, snapshot_)
import FRP.Reactive.LegacyAdapters (Action, makeClock, cGetTime,
                                    makeEvent, mkUpdater)

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode(ExitSuccess))


import SDLUtils (loadImage, setColorKey)
import Utils (takeWhileM)

data UI =
  UI { leftPressed  :: Behavior Bool
     , rightPressed :: Behavior Bool
     , upPressed    :: Behavior Bool
     , downPressed  :: Behavior Bool
     , beat         :: Event () }

data Dot =
  Dot { position :: Behavior (Int, Int) }

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
ticksPerFrame   = 1000 `div` framesPerSecond

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen   <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  white    <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff
  dotImage <- (`setColorKey` (0xff, 0xff, 0xff)) =<< loadImage "dot.bmp"

  let context = Context screen white dotImage

  SDL.setCaption "Move the Dot" ""

  clock <- makeClock
  (leftPressSink,  leftPressE)      <- makeEvent clock
  (rightPressSink, rightPressE)     <- makeEvent clock
  (upPressSink,    upPressE)        <- makeEvent clock
  (downPressSink,  downPressE)      <- makeEvent clock

  (leftReleaseSink,  leftReleaseE)  <- makeEvent clock
  (rightReleaseSink, rightReleaseE) <- makeEvent clock
  (upReleaseSink,    upReleaseE)    <- makeEvent clock
  (downReleaseSink,  downReleaseE)  <- makeEvent clock

  (beatSink,         beatE)         <- makeEvent clock

  let ui  = UI (flipFlop leftPressE  leftReleaseE)
               (flipFlop rightPressE rightReleaseE)
               (flipFlop upPressE    upReleaseE)
               (flipFlop downPressE  downReleaseE)
               beatE

  let dot = uiToDot ui

  updater <- mkUpdater (cGetTime clock) (dotToAction context dot)

  fix $ \loop -> do
    events <- takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

    forM_ events $ \event -> do
      case event of
        SDL.Quit           -> quit

        SDL.KeyDown keysym ->
          case SDL.symKey keysym of
            SDL.SDLK_LEFT  -> leftPressSink ()
            SDL.SDLK_RIGHT -> rightPressSink ()
            SDL.SDLK_UP    -> upPressSink ()
            SDL.SDLK_DOWN  -> downPressSink ()
            _              -> return ()

        SDL.KeyUp   keysym ->
          case SDL.symKey keysym of
            SDL.SDLK_LEFT  -> leftReleaseSink ()
            SDL.SDLK_RIGHT -> rightReleaseSink ()
            SDL.SDLK_UP    -> upReleaseSink ()
            SDL.SDLK_DOWN  -> downReleaseSink ()
            _              -> return ()
        _                  -> return ()

    SDL.delay ticksPerFrame
    beatSink ()

    updater
    loop

  where quit   = SDL.quit >> exitWith ExitSuccess

uiToDot :: UI -> Dot
uiToDot (UI left right up down beat) = Dot position
  where vx :: Behavior Int
        vx = keysToVelocity <$> left <*> right

        vy :: Behavior Int
        vy = keysToVelocity <$> up <*> down

        keysToVelocity :: Bool -> Bool -> Int
        keysToVelocity True  _     = -10
        keysToVelocity False True  =  10
        keysToVelocity False False =   0

        v :: Behavior (Int, Int)
        -- Bounding does not work.
        -- Additional information:
        --   http://trac.haskell.org/reactive/ticket/1
        -- v = bounded $ (,) <$> vx <*> vy
        v = (,) <$> vx <*> vy

        position :: Behavior (Int, Int)
        position = sumB $ v `snapshot_` beat

        bounded :: Behavior (Int, Int) -> Behavior (Int, Int)
        bounded velocity =
          boundVelocity <$> (fmap boundPos position) <*> velocity
          where bound top value = (0 `max` value) `min` top
                boundPos        = bound screenWidth *** bound screenHeight

                boundVelocity (px, py) (vx, vy) =
                  let vx' = if px == 0 || px == screenWidth then 0 else vx
                      vy' = if py == 0 || py == screenHeight then 0 else vy
                  in (vx', vy')

dotToAction :: Context -> Dot -> Behavior Action
dotToAction (Context screen white dot) (Dot position) =
  fmap (\position -> fillBg >> drawDot position >> SDL.flip screen) position
  where fillBg         = SDL.fillRect screen Nothing white
        drawDot (x, y) =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dx) (y - dy) 0 0)
        dx             = dotWidth `div` 2
        dy             = dotHeight `div` 2
