{-# LANGUAGE ViewPatterns #-}

module Main (main)
       where

import Control.Monad (when)

import Data.Function (fix)
import Data.Maybe (fromJust)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image

screenWidth  = 640
screenHeight = 480
screenBPP    = 32

buttonClips = [SDL.Rect x y 320 240 | y <- [0, 240], x <- [0, 320]]
buttonRect  = SDL.Rect 170 120 320 240

clipMouseOver = 0
clipMouseOut  = 1
clipMouseDown = 2
clipMouseUp   = 3

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  SDL.setCaption "button is not a miracle" ""

  buttonSheet <- loadImage "button.png"
  bg          <- loadImage "background.png"

  SDL.blitSurface bg Nothing screen Nothing
  applyButton buttonSheet clipMouseOut screen
  SDL.flip screen

  fix $ \loop -> do
    event <- SDL.waitEventBlocking

    case event of
      SDL.Quit -> return ()
      _        -> do
        let clip = case event of
                     SDL.MouseMotion x y _ _                ->
                       if insideButton x y
                       then Just clipMouseOver
                       else Just clipMouseOut
                     SDL.MouseButtonDown x y SDL.ButtonLeft ->
                       if insideButton x y
                       then Just clipMouseDown
                       else Just clipMouseOut
                     SDL.MouseButtonUp x y SDL.ButtonLeft   ->
                       if insideButton x y
                       then Just clipMouseUp
                       else Just clipMouseOut
                     _                                      ->
                       Nothing
        when (clip /= Nothing) $ do
          applyButton buttonSheet (fromJust clip) screen
          SDL.flip screen
        loop

  SDL.quit

  where insideButton (fromIntegral -> x) (fromIntegral -> y) =
          (x > SDL.rectX buttonRect) &&
          (x < SDL.rectX buttonRect + SDL.rectW buttonRect) &&
          (y > SDL.rectY buttonRect) &&
          (y < SDL.rectY buttonRect + SDL.rectH buttonRect)

        applyButton sheet clip dst =
          SDL.blitSurface sheet (Just $ buttonClips !! clip)
                          dst   (Just buttonRect)

loadImage path = do
  image   <- Image.load path
  dfImage <- SDL.displayFormat image

  return dfImage
