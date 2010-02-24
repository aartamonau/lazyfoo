module Main (main)
       where

import Control.Monad (when)

import Data.Function (fix)
import Data.Maybe (fromJust)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified Graphics.UI.SDL.TTF as TTF

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

textColor    = SDL.Color 0 0 0

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "life is a miracle" ""

  font         <- TTF.openFont "DejaVuSansMono.ttf" 28
  bg           <- loadImage "background.png"
  upMessage    <- TTF.renderTextSolid font "up"    textColor
  downMessage  <- TTF.renderTextSolid font "down"  textColor
  leftMessage  <- TTF.renderTextSolid font "left"  textColor
  rightMessage <- TTF.renderTextSolid font "right" textColor
  helpMessage  <- TTF.renderTextSolid font "press arrow keys" textColor

  applySurface 0 0 bg screen
  applyMessage screen helpMessage
  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.KeyDown keysym -> do
        let message = case SDL.symKey keysym of
                        SDL.SDLK_UP    -> Just upMessage
                        SDL.SDLK_DOWN  -> Just downMessage
                        SDL.SDLK_LEFT  -> Just leftMessage
                        SDL.SDLK_RIGHT -> Just rightMessage
                        _              -> Nothing

        when (message /= Nothing) $ do
          applySurface 0 0 bg screen
          applyMessage screen (fromJust message)
          SDL.flip screen
        loop
      SDL.Quit -> return ()
      _        -> loop


  TTF.quit
  SDL.quit

  where applyMessage surface message = do
          SDL.Rect _ _ w h <- SDL.getClipRect message
          applySurface ((screenWidth - w) `div` 2)
                       ((screenHeight - h) `div` 2)
                       message surface

loadImage path = do
  image   <- Image.load path
  dfImage <- SDL.displayFormat image

  return dfImage

applySurface x y src dst = do
  SDL.blitSurface src Nothing dst (Just $ SDL.Rect x y 0 0)
