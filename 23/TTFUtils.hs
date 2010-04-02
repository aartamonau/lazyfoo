module TTFUtils ( applyText )
       where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

applyText :: String -> TTF.Font -> SDL.Color -> Int -> SDL.Surface -> IO ()
applyText text font color y dst = do
  message <- TTF.renderTextSolid font text color
  SDL.Rect _ _ w  _ <- SDL.getClipRect message
  SDL.Rect _ _ dw _ <- SDL.getClipRect dst

  SDL.blitSurface message Nothing dst (Just $ SDL.Rect ((dw - w) `div` 2) y 0 0)
  return ()