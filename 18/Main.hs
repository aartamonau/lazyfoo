{-# LANGUAGE Arrows, PatternGuards #-}

module Main (main)
       where

import Control.Arrow (returnA, arr, (&&&), (>>>))
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import qualified Data.Map as M
import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  isEvent, filterE, tag, repeatedly,
                  rMerge, hold, dAccumHoldBy)
import qualified Graphics.UI.SDL as SDL

import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

framesPerSecond = 30
ticksPerFrame   = 1 / framesPerSecond

dotDiameter     = 20

dotStartingX    = 10
dotStartingY    = 10

dotBoxes = [ ( 6, 1),
             (10, 1),
             (14, 1),
             (16, 2),
             (18, 2),
             (20, 6),
             (18, 2),
             (16, 2),
             (14, 1),
             (10, 1),
             ( 6, 1) ]

data Dot = Dot Int Int

collisionDot = Dot 30 30

data ArrowKey = UpKey | DownKey | LeftKey | RightKey
              deriving (Show, Eq)

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , closed  :: Bool
        }

data GContext =
  GContext { c_screen    :: SDL.Surface
           , c_bgColor   :: SDL.Pixel
           , c_dot       :: SDL.Surface }

initSDL :: IO GContext
initSDL = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  white  <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff
  dot    <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "dot.bmp"

  SDL.setCaption "Move the Dot" ""

  return $ GContext screen white dot

quitSDL :: IO ()
quitSDL = SDL.quit

renderScreen :: GContext -> Dot -> IO ()
renderScreen (GContext screen white dot) (Dot x y) = do
  fillBg
  drawDot x y
  drawDot cx cy
  SDL.flip screen

  where fillBg      = SDL.fillRect screen Nothing white
        drawDot x y =
          SDL.blitSurface dot Nothing
                          screen (Just $ SDL.Rect (x - dotR) (y - dotR) 0 0)
        dotR = dotDiameter `div` 2
        Dot cx cy = collisionDot

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
                  case handleKeysym keysym of
                    Just arrow -> Just $ Input (Event arrow) NoEvent False
                    _          -> Nothing
                SDL.KeyUp keysym ->
                  case handleKeysym keysym of
                    Just arrow -> Just $ Input NoEvent (Event arrow) False
                    _          -> Nothing
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        time <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.stop timer
        Timer.start timer

        if isNothing input
          then return (time, Just defaultInput)
          else return (time, input)

  let actuate Nothing _  = return True
      actuate (Just dot) beat
        | isEvent beat = renderScreen gcontext dot >> return False
        | otherwise    = return False

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (dot beat &&& beat)

  quitSDL

  where arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        defaultInput = Input NoEvent NoEvent False

dot :: SF () (Event ()) -> SF Input (Maybe Dot)
dot beat =
  proc input@(Input down up closed) -> do
    let leftD  = filterE (== LeftKey) down
    let leftU  = filterE (== LeftKey) up
    let rightD = filterE (== RightKey) down
    let rightU = filterE (== RightKey) up

    let upD   = filterE (== UpKey) down
    let upU   = filterE (== UpKey) up
    let downD = filterE (== DownKey) down
    let downU = filterE (== DownKey) up

    vl <- v -< (leftD, leftU)
    vr <- v -< (rightD, rightU)
    vu <- v -< (upD, upU)
    vd <- v -< (downD, downU)

    let vx = vr - vl
    let vy = vd - vu

    beat' <- beat -< ()

    rec
      (rvx, rvy) <-
        arr fst &&& respectBoundaries >>> avoidCollision -< ((x, y), (vx, vy))

      x <- dAccumHoldBy (+) dotStartingX -< beat' `tag` rvx
      y <- dAccumHoldBy (+) dotStartingY -< beat' `tag` rvy

    returnA -< if closed then Nothing else Just (Dot x y)

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dv
                        `rMerge`
                        deactivate `tag` 0

        dotRadius = dotDiameter `div` 2
        dv        = 1

        respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
        respectBoundaries =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if breaksBoundaries (x + vx, y) then 0 else vx
            let rvy = if breaksBoundaries (x, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        breaksBoundaries :: (Int, Int) -> Bool
        breaksBoundaries (x, y) =
          or [left < 0, top < 0, bottom > screenHeight, right > screenWidth]
          where left   = x - dotRadius
                right  = x + dotRadius
                top    = y - dotRadius
                bottom = y + dotRadius

        rectsHaveCollision :: SDL.Rect -> SDL.Rect -> Bool
        rectsHaveCollision a b =
          not $ or [aBottom <= bTop,
                    aTop    >= bBottom,
                    aLeft   >= bRight,
                    aRight  <= bLeft]
          where borders :: SDL.Rect -> (Int, Int, Int, Int)
                borders (SDL.Rect x y w h) = (x, x + w, y, y + h)

                (aLeft, aRight, aTop, aBottom) = borders a
                (bLeft, bRight, bTop, bBottom) = borders b

        dotRects :: (Int, Int) -> [SDL.Rect]
        dotRects (x, y) =
          [SDL.Rect (x - w `div` 2) (top + offset) w h
            | (offset, (w, h)) <- boxes]
          where top       = y - dotDiameter `div` 2
                boxes     = zip (scanl (+) 0 (map snd dotBoxes)) dotBoxes

        collisionDotRects | Dot cx cy <- collisionDot = dotRects (cx, cy)

        checkCollision :: (Int, Int) -> Bool
        checkCollision (x, y) =
          or [rectsHaveCollision a b | a <- rects, b <- collisionDotRects]
          where rects = dotRects (x, y)

        avoidCollision :: SF ((Int, Int), (Int, Int)) (Int, Int)
        avoidCollision =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if checkCollision (x + vx,  y) then 0 else vx
            let rvy = if checkCollision (x + rvx, y + vy) then 0 else vy

            returnA -< (rvx, rvy)
