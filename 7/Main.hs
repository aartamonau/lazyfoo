module Main (main)
       where

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import qualified Graphics.UI.SDL.TTF as TTF

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

textColor    = SDL.Color 255 255 255

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  TTF.init

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "life is a miracle" ""

  bg      <- loadImage "background.png"
  font    <- TTF.openFont "DejaVuSansMono.ttf" 28

  message <- TTF.renderTextSolid font "life is a miracle" textColor

  applySurface 0   0      bg screen Nothing
  applySurface 0 150 message screen Nothing

  SDL.flip screen

  TTF.closeFont font

  waitForClick

  TTF.quit
  SDL.quit


waitForClick = do
  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.MouseButtonUp _ _ SDL.ButtonLeft -> return ()
      _                                    -> loop

loadImage path = do
  image   <- Image.load path
  dfImage <- SDL.displayFormat image

  return dfImage

applySurface x y src dst clip = do
  SDL.blitSurface src clip dst (Just $ SDL.Rect x y 0 0)
