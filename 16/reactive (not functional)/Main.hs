{-# LANGUAGE TupleSections #-}

module Main ( main )
       where

import Debug.Trace


import Control.Applicative (pure, (<*>))
import Control.Arrow ((***))
import Control.Monad (forM_, when)

import Data.Either (either)
import Data.Function (fix)
import qualified Data.Map as M

import FRP.Reactive (Event, Behavior,
                     eitherE, filterE,
                     stepper, integral,
                     sumB, snapshot_, joinMaybes)
import FRP.Reactive.LegacyAdapters (Action, makeClock, cGetTime,
                                    makeEvent, mkUpdater)

import qualified Graphics.UI.SDL as SDL

import System (exitWith, ExitCode(ExitSuccess))


import SDLUtils (loadImage, setColorKey)
import Utils (takeWhileM)

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data UI =
  UI { keyDown :: Event ArrowKey
     , keyUp   :: Event ArrowKey
     , beat    :: Event () }

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

framesPerSecond = 20
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
  (keyDownSink, keyDownE) <- makeEvent clock
  (keyUpSink,   keyUpE)   <- makeEvent clock
  (beatSink,    beatE)    <- makeEvent clock

  let ui  = UI keyDownE keyUpE beatE
  let dot = uiToDot ui

  updater <- mkUpdater (cGetTime clock) (dotToAction context dot)

  fix $ \loop -> do
    events <- takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

    forM_ events $ \event -> do
      case event of
        SDL.Quit           -> quit
        SDL.KeyDown keysym ->
          sinkArrow keyDownSink keysym
        SDL.KeyUp   keysym ->
          sinkArrow keyUpSink keysym
        _                  -> return ()

    SDL.delay ticksPerFrame
    beatSink ()

    updater
    loop

  where quit   = SDL.quit >> exitWith ExitSuccess
        arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        sinkArrow sink keysym = do
          let key = M.lookup (SDL.symKey keysym) arrows
          case key of
            Nothing    -> return ()
            Just arrow -> sink arrow

uiToDot :: UI -> Dot
uiToDot (UI down up beat) = Dot $ fmap ((,100)) $ sumB (snapshot_ vx beat)
  -- Dot $ fmap (\x -> (x, x)) vx
  where vx :: Behavior Int
        -- vx = pure 1
        vx = stepper 0 $
               fmap v $ eitherE -- down up
                              (filterE (== LeftKey) down) (filterE (== LeftKey) up)
               where v = either (const 10) (const 0)

dotToAction :: Context -> Dot -> Behavior Action
dotToAction (Context screen white dot) (Dot position) =
  fmap (\position -> fillBg >> drawDot position >> SDL.flip screen) position
  where fillBg         = SDL.fillRect screen Nothing white
        drawDot (x, y) =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dx) (y - dy) 0 0)
        dx             = dotWidth `div` 2
        dy             = dotHeight `div` 2