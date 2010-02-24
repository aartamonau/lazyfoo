module Main ( main )
       where

import Control.Monad (forM_, when)

import Data.Function (fix)
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

import System (exitWith, ExitCode (ExitSuccess))


import qualified Timer
import Utils (takeWhileM)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "Frame Rate Test" ""

  bg <- loadImage "testing.png"

  timer <- Timer.mkTimer
  frame <- newIORef 0

  fix $ \loop -> do
    Timer.start timer

    events <- takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

    forM_ events $ \event -> do
      case event of
        SDL.Quit -> quit
        _        -> return ()


    SDL.blitSurface bg Nothing screen Nothing
    SDL.flip screen
    modifyIORef frame (+1)

    ticks  <- Timer.getTicks timer
    frames <- readIORef frame
    when (ticks > 1000) $ do
      let secs = (fromIntegral ticks) / 1000
      let fps  = (fromIntegral frames) / secs

      SDL.setCaption ("Average Frames Per Second: " ++ show fps) ""

      Timer.stop timer
      writeIORef frame 0

    loop

  quit

  where quit = SDL.quit >> exitWith ExitSuccess

loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage
