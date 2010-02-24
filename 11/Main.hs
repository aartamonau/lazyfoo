module Main (main)
       where

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG
import qualified Graphics.UI.SDL.Mixer as MIX
import qualified Graphics.UI.SDL.TTF as TTF

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

textColor    = SDL.Color 0 0 0

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init
  MIX.openAudio MIX.defaultFrequency MIX.AudioS16Sys 2 4096

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "when the music's over" ""

  bg      <- loadImage "background.png"
  -- font    <- TTF.openFont "DejaVuSansMono.ttf" 30
  font    <- TTF.openFont "lazy.ttf" 30

  -- music   <- MIX.loadMUS "beat.wav"
  music   <- MIX.loadMUS "jimi.ogg"
  scratch <- MIX.loadWAV "scratch.wav"
  high    <- MIX.loadWAV "high.wav"
  medium  <- MIX.loadWAV "medium.wav"
  low     <- MIX.loadWAV "low.wav"

  applySurface 0 0 bg screen

  let cx = screenWidth `div` 2

  applyText "Press 1, 2, 3, or 4 to play a sound effect" font cx 100 screen
  applyText "Press 9 to play or pause the music"         font cx 200 screen
  applyText "Press 0 to stop the music"                  font cx 300 screen

  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.Quit           -> return ()
      SDL.KeyDown keysym -> do
        case (SDL.symKey keysym) of
          SDL.SDLK_1 -> MIX.playChannel (-1) scratch 0 >> return ()
          SDL.SDLK_2 -> MIX.playChannel (-1)    high 0 >> return ()
          SDL.SDLK_3 -> MIX.playChannel (-1)  medium 0 >> return ()
          SDL.SDLK_4 -> MIX.playChannel (-1)     low 0 >> return ()
          SDL.SDLK_9 -> do
            playing <- MIX.playingMusic
            if playing
              then do
                paused <- MIX.pausedMusic
                if paused
                  then MIX.resumeMusic
                  else MIX.pauseMusic
              else MIX.playMusic music (-1)
          SDL.SDLK_0 -> MIX.haltMusic
          _          -> return ()
        loop
      _ -> loop

  MIX.closeAudio
  TTF.quit
  SDL.quit

loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage

applySurface x y src dst = do
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)

applyText text font x y dst = do
  message <- TTF.renderTextSolid font text textColor
  SDL.Rect _ _ w _ <- SDL.getClipRect message

  applySurface (x - w `div` 2) y message dst
