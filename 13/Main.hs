module Main ( main )
       where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (when, forever)

import Data.Function (fix)

import Foreign.Ptr (nullPtr)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import qualified Graphics.UI.SDL.TTF as TTF

import qualified Timer

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

textColor       = SDL.Color 0 0 0
timerResolution = 1000 * 10

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init

  timer       <- Timer.mkTimer
  timerThread <-
    forkIO $ forever $ do
      SDL.pushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr
      threadDelay timerResolution

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "time keeps on slipping" ""

  bg   <- loadImage "background.png"
  font <- TTF.openFont "DejaVuSansMono.ttf" 27

  let redraw = do
        applySurface 0 0 bg screen
        applyText "Press S to start or stop the timer"    font 200 screen
        applyText "Press P to pause or unpause the timer" font 250 screen
        return ()

  redraw
  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEvent

    case event of
      SDL.Quit         -> return ()
      SDL.User _ _ _ _ -> do
        started <- Timer.isStarted timer
        when started $ do
          ticks <- Timer.getTicks timer
          redraw
          applyText ("Timer: " ++ show ticks) font 50 screen
          SDL.flip screen
        loop
      SDL.KeyDown keysym -> do
        case (SDL.symKey keysym) of
          SDL.SDLK_s -> Timer.toggleStart timer
          SDL.SDLK_p -> Timer.togglePause timer
          _          -> return ()
        loop
      _                  -> loop

  killThread timerThread
  TTF.quit
  SDL.quit

loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage

applySurface x y src dst = do
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)

applyText text font y dst = do
  message <- TTF.renderTextSolid font text textColor
  SDL.Rect _ _ w _ <- SDL.getClipRect message

  applySurface ((screenWidth - w) `div` 2) y message dst
