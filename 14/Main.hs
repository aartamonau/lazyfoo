module Main ( main )
       where

import Control.Monad (when, forM_, forever)

import Data.Function (fix)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Word (Word32)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import qualified Graphics.UI.SDL.TTF as TTF

import System (exitWith, ExitCode (..))

import qualified Timer

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

textColor       = SDL.Color 0 0 0

framesPerSecond = 25

ticksPerFrame :: Word32
ticksPerFrame   = 1000 `div` (fromIntegral framesPerSecond)

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init

  timer  <- Timer.mkTimer
  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "time keeps on slipping" ""

  bg      <- loadImage "background.png"
  font    <- TTF.openFont "lazy.ttf" 50

  message <- TTF.renderTextSolid font "Testing Frame Rate" textColor
  SDL.Rect _ _ messageW messageH <- SDL.getClipRect message

  let cx           = (screenWidth - messageW) `div` 2
  let redraw frame = do
        applySurface 0 0 bg screen
        SDL.blitSurface message Nothing screen textRect
        SDL.flip screen
        where textRect = Just $ SDL.Rect cx y 0 0
              y        = frame *
                          ((screenHeight + messageH * 2) `div` framesPerSecond)

  cap   <- newIORef True
  frame <- newIORef 0

  fix $ \loop -> do
    Timer.start timer

    events <- takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

    forM_ events $ \event -> do
      case event of
        SDL.Quit           -> quit
        SDL.KeyDown keysym -> do
          when (SDL.symKey keysym == SDL.SDLK_RETURN) $
            modifyIORef cap not
        _                  -> return ()

    redraw =<< readIORef frame
    modifyIORef frame ((`mod` framesPerSecond) . (+1))

    cap   <- readIORef cap
    ticks <- Timer.getTicks timer
    when (cap && ticks < ticksPerFrame) $
      SDL.delay (ticksPerFrame - ticks)

    Timer.stop timer
    loop

  quit

  where takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
        takeWhileM p = foldr (liftM2' (:) []) (return [])
          where liftM2' f z mx my = do
                  x <- mx
                  if p x
                    then do
                      y <- my
                      return $ f x y
                    else
                      return z
        quit :: IO ()
        quit = do
          TTF.quit
          SDL.quit
          exitWith ExitSuccess

loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage

applySurface x y src dst = do
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)
