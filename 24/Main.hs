{-# LANGUAGE Arrows #-}

module Main (main)
       where

import Control.Arrow (returnA, (&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Data.Maybe (isNothing)
import Data.Word (Word8)

import FRP.Yampa (Event(..), SF,
                  filterE, tag, repeatedly,
                  rMerge, hold, dAccumHoldBy)
import qualified Graphics.UI.SDL as SDL

import System.Directory (doesFileExist)

import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

framesPerSecond = 30
ticksPerFrame   = 1 / framesPerSecond

data Dot =
  Dot { x  :: Int
      , y  :: Int
      , bg :: (Word8, Word8, Word8) }
  deriving (Show, Read)

white = (0xff, 0xff, 0xff)
red   = (0xff, 0x00, 0x00)
green = (0x00, 0xff, 0x00)
blue  = (0x00, 0x00, 0xff)

dotD            = 20
dotR            = dotD `div` 2

data Input =
  Input { keyDown :: Event SDL.SDLKey
        , keyUp   :: Event SDL.SDLKey
        , closed  :: Bool
        }

data GContext =
  GContext { c_screen :: SDL.Surface
           , c_dot    :: SDL.Surface }

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  dot    <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "dot.png"

  SDL.setCaption "Move the Dot" ""

  return $ GContext screen dot

quitSDL :: IO ()
quitSDL = SDL.quit

renderScreen :: GContext -> Dot -> IO ()
renderScreen (GContext screen dot) (Dot x y (r, g, b)) = do
  bg <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b

  fillBg bg
  drawDot x y
  SDL.flip screen

  where fillBg bg   = SDL.fillRect screen Nothing bg
        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dotR) (y - dotR) 0 0)


main :: IO ()
main = do
  gcontext <- initSDL

  timer <- Timer.mkTimer
  let init  = Timer.start timer >> return (Input NoEvent NoEvent False)

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.Quit           -> Just $ Input NoEvent NoEvent True
                SDL.KeyDown keysym ->
                  Just $ Input (Event $ SDL.symKey keysym) NoEvent False
                SDL.KeyUp keysym ->
                  Just $ Input NoEvent (Event $ SDL.symKey keysym) False
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        if isNothing input
          then return (time, Just defaultInput)
          else return (time, input)

  let actuate (Left dot)  _       = do
        writeFile "game_save" (show dot)
        return True
      actuate _           NoEvent = return False
      actuate (Right dot) _       = do
        renderScreen gcontext dot
        return False

  let beat = repeatedly ticksPerFrame ()

  saveExists <- doesFileExist "game_save"
  startingDot <- if saveExists
                   then fmap read (readFile "game_save")
                   else return $ Dot 10 10 white

  reactimate init sense (uncurry actuate) (dot startingDot beat &&& beat)

  quitSDL

  where defaultInput = Input NoEvent NoEvent False

-- Left dot -- stop program execution
-- Right dot -- continue execution
dot :: Dot -> SF () (Event ()) -> SF Input (Either Dot Dot)
dot (Dot sx sy scolor) beat =
  proc (Input down up closed) -> do
    let leftD  = filterE (== SDL.SDLK_LEFT)  down
    let leftU  = filterE (== SDL.SDLK_LEFT)  up
    let rightD = filterE (== SDL.SDLK_RIGHT) down
    let rightU = filterE (== SDL.SDLK_RIGHT) up

    let upD    = filterE (== SDL.SDLK_UP)    down
    let upU    = filterE (== SDL.SDLK_UP)    up
    let downD  = filterE (== SDL.SDLK_DOWN)  down
    let downU  = filterE (== SDL.SDLK_DOWN)  up

    let colorSwitchE = filterE (`elem` [SDL.SDLK_1, SDL.SDLK_2,
                                        SDL.SDLK_3, SDL.SDLK_4])
                               down

    color <- hold scolor -< fmap chooseColor colorSwitchE

    vl <- v -< (leftD, leftU)
    vr <- v -< (rightD, rightU)
    vu <- v -< (upD, upU)
    vd <- v -< (downD, downU)

    let vx = vr - vl
    let vy = vd - vu

    beat' <- beat -< ()

    rec
      (rvx, rvy) <- respectBoundaries -< ((x, y), (vx, vy))

      x <- dAccumHoldBy (+) sx -< beat' `tag` rvx
      y <- dAccumHoldBy (+) sy -< beat' `tag` rvy

    let dot = Dot x y color
    returnA -< if closed then Left dot else Right dot

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dv
                        `rMerge`
                        deactivate `tag` 0

        dv = dotR

        respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
        respectBoundaries =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if breaksBoundaries (x + vx, y) then 0 else vx
            let rvy = if breaksBoundaries (x, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        breaksBoundaries :: (Int, Int) -> Bool
        breaksBoundaries (x, y) =
          or [left < 0, top < 0, bottom > screenHeight, right > screenWidth]
          where left   = x - dotR
                right  = x + dotR
                top    = y - dotR
                bottom = y + dotR

        chooseColor SDL.SDLK_1 = white
        chooseColor SDL.SDLK_2 = red
        chooseColor SDL.SDLK_3 = green
        chooseColor SDL.SDLK_4 = blue
