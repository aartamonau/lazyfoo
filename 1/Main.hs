module Main (main)
       where

import Control.Applicative
import Control.Arrow

import Control.Monad

import Data.Function

import qualified Graphics.UI.SDL as SDL

import System
import System.FilePath.Glob

main = do
  patterns <- map compile <$> getArgs
  images   <- concat . fst <$> globDir patterns "."

  SDL.init [SDL.InitEverything]

  forM_ images $ \path -> do
    image  <- SDL.loadBMP path
    (w, h) <- --((800 `min`) *** (600 `min`)) <$>
              (min 800 . SDL.rectW &&& min 600 . SDL.rectH) <$> SDL.getClipRect image
    screen <- SDL.setVideoMode w h 32 [SDL.SWSurface]
    SDL.blitSurface image Nothing screen Nothing
    SDL.flip screen

    SDL.freeSurface image

    fix $ \loop -> do
      event <- SDL.waitEventBlocking

      case event of
        SDL.MouseButtonUp _ _ SDL.ButtonLeft  -> return ()
        SDL.MouseButtonUp _ _ SDL.ButtonRight ->
          SDL.quit >> exitWith ExitSuccess
        _                                     -> loop

  SDL.quit