module Main (main)
       where

import Control.Applicative ((<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, forever)

import Data.Function (fix)
import Data.IORef (newIORef, readIORef, writeIORef)

import Foreign.Ptr (nullPtr)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import qualified Graphics.UI.SDL.TTF as TTF

import System.Timeout (timeout)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

textColor       = SDL.Color 0 0 0

timerResolution = 1000 * 10

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "time keeps on slipping" ""

  bg   <- loadImage "background.png"
  font <- TTF.openFont "DejaVuSansMono.ttf" 30

  let redraw = do
        applySurface 0 0 bg screen
        applyText "Press S to start or stop the timer" font
                  (screenHeight `div` 2) screen
        applyText "Press C to clear the screen" font
                  (screenHeight `div` 2 + 75) screen
        return ()

  redraw

  SDL.flip screen

  (timerKill, timerDead) <- forkTimerThread timerResolution

  timerOnRef    <- newIORef False
  timerStartRef <- newIORef =<< SDL.getTicks

  fix $ \loop -> do
    event <- SDL.waitEvent

    case event of
      SDL.Quit           -> return ()
      SDL.KeyDown keysym -> do
        case (SDL.symKey keysym) of
          SDL.SDLK_s -> do
            timerOn <- readIORef timerOnRef

            if timerOn
              then writeIORef timerOnRef False
              else do
                writeIORef timerOnRef True
                writeIORef timerStartRef =<< SDL.getTicks
          SDL.SDLK_c -> do
            redraw
            SDL.flip screen
          _          -> return ()
        loop
      SDL.User _ _ _ _   -> do
        timerOn <- readIORef timerOnRef
        when timerOn $ do
          start    <- readIORef timerStartRef
          current  <- SDL.getTicks
          let diff = current - start
          let str  = "Timer: " ++ show diff

          redraw
          applyText str font 50 screen
          SDL.flip screen

        loop
      _                  -> loop

  putMVar  timerKill ()
  takeMVar timerDead

  TTF.quit
  SDL.quit

forkTimerThread resolution = do
  kill <- newEmptyMVar
  dead <- newEmptyMVar

  forkIO $ fix $ \loop -> do
    signaled <- timeout resolution $ takeMVar kill

    case signaled of
      Nothing -> do
        SDL.pushEvent $ SDL.User SDL.UID0 0 nullPtr nullPtr
        loop
      _       -> putMVar dead ()

  return (kill, dead)

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
