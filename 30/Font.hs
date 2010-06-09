{-# LANGUAGE RecordWildCards #-}

module Font -- ( Font, createFromFile, create, showText )
       where


import Control.Monad ( forM )

import Data.Array ( Array, listArray, (!) )
import Data.Char ( ord )
import Data.Word ( Word8 )


import qualified Graphics.UI.SDL as SDL


import SDLUtils ( loadImage, getPixel, setColorKey )


data Font =
  Font { bitmap  :: SDL.Surface
       , chars   :: Array Word8 SDL.Rect
       , newLine :: Int
       , space   :: Int
       } deriving Show

createFromFile :: FilePath -> IO Font
createFromFile path =
  loadImage path >>= flip setColorKey (0x00, 0xff, 0xff) >>= create

create :: SDL.Surface -> IO Font
create bitmap = do
  let format = SDL.surfaceGetPixelFormat bitmap
  colorKey  <- SDL.mapRGB format 0x00 0xff 0xff
  let findNonBg' = findNonBg colorKey

  let width  = SDL.surfaceGetWidth bitmap
  let height = SDL.surfaceGetHeight bitmap
  let cellW  = width `div` 16
  let cellH  = height `div` 16

  bounds <- forM (cartesian [0 .. 15] [0 .. 15]) $ \(row, col) -> do
    let cornerX = col * cellW
    let cornerY = row * cellH
    let corner  = (cornerX, cornerY)

    top    <- findNonBg' $ shift corner $ cartesianRev (0 ... cellW - 1)
                                                       (0 ... cellH - 1)

    bottom <- findNonBg' $ shift corner $ cartesianRev (0 ... cellW - 1)
                                                       (cellH - 1 ... 0)

    return (maybe cellH ((flip (-) cornerY) . snd) top,
            maybe (cellH - 1) ((flip (-) cornerY) . snd) bottom)

  let top     = minimum $ map fst bounds
  let bottomA = map snd bounds !! ord 'A'

  chars <- forM (cartesian [0 .. 15] [0 .. 15]) $ \(col, row) -> do
    let cornerX = row * cellW
    let cornerY = col * cellH
    let corner  = (cornerX, cornerY)

    left  <- findNonBg' $ shift corner $ cartesian (0 ... cellW - 1)
                                                   (0 ... cellH - 1)

    right <- findNonBg' $ shift corner $ cartesian (cellW - 1 ... 0)
                                                   (0 ... cellH - 1)

    let x = maybe cornerX fst left
    let y = cornerY + top
    let w = maybe cellW ((+1) . (flip (-) x) . fst) right
    let h = cellH - top

    return $ SDL.Rect x y w h

  return $ Font bitmap (listArray (0, maxBound) chars)
                (bottomA - top) (cellW `div` 2)

  where findNonBg :: SDL.Pixel -> [(Int, Int)] -> IO (Maybe (Int, Int))
        findNonBg bg ps = go ps
          where go []       = return Nothing
                go (p : ps) = do
                  matches <- pred p
                  if matches
                    then return $ Just p
                    else go ps

                pred = fmap (/= bg) . (uncurry $ getPixel bitmap)

        cartesian xs ys    = [ (x, y) | x <- xs, y <- ys ]
        cartesianRev xs ys = [ (x, y) | y <- ys, x <- xs ]

        shift (ox, oy) = map (\(x, y) -> (x + ox, y + oy))

        infixl 0 ...
        x ... y | x <= y    = take (y - x + 1) $ iterate succ x
                | otherwise = take (x - y + 1) $ iterate pred x

showText :: Font -> String -> SDL.Surface -> Int -> Int -> IO ()
showText (Font { .. }) text surface sx sy = go text sx sy
  where go []       _ _ = return ()
        go (c : cs) x y
          | c == '\n' = go cs sx (y + newLine)
          | c == ' '  = go cs (x + space) y
          | otherwise = do
            SDL.blitSurface bitmap (Just cRect)
                            surface (Just $ SDL.Rect x y 0 0)
            go cs (x + w + 1) y
          where word = fromIntegral . ord
                cRect@(SDL.Rect _ _ w _) = chars ! (word c)
