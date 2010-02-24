{-# LANGUAGE ParallelListComp #-}

module Main (main)
       where

import Control.Monad (forM_)

import Data.Function (fix)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL.Image

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

rects = [SDL.Rect x y 100 100 | y <- [0, 100], x <- [0, 100]]

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  let screenFormat = SDL.surfaceGetPixelFormat screen
  white <- SDL.mapRGB screenFormat 0xff 0xff 0xff
  SDL.fillRect screen Nothing white

  SDL.setCaption "dots" ""


  dots <- loadImageWithColorkey "dots.png" (0x0, 0xff, 0xff)

  forM_ [(x, y, r) | y <- [0, 380], x <- [0, 540]
                   | r <- rects] $ \(x, y, r) ->
    applySurface x y dots screen (Just r)


  SDL.flip screen

  waitForClick

  SDL.quit

waitForClick = do
  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.MouseButtonUp _ _ SDL.ButtonLeft -> return ()
      _                                    -> loop

loadImage path = do
  image   <- SDL.Image.load path
  dfImage <- SDL.displayFormat image

  return dfImage

setColorKey surface (r, g, b) = do
  let format =  SDL.surfaceGetPixelFormat surface
  colorkey   <- SDL.mapRGB format r g b

  SDL.setColorKey surface [SDL.SrcColorKey] colorkey

loadImageWithColorkey path colorkey = do
  image <- loadImage path
  setColorKey image colorkey

  return image

applySurface x y src dst clip = do
  SDL.blitSurface src clip dst (Just $ SDL.Rect x y 0 0)
