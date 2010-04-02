module SDLUtils ( loadImage )
       where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as IMG

loadImage :: FilePath -> IO SDL.Surface
loadImage path = do
  image   <- IMG.load path
  dfImage <- SDL.displayFormat image

  return dfImage
