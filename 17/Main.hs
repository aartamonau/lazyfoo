{-# LANGUAGE Arrows #-}

module Main (main)
       where

import Control.Arrow (Arrow,
                      (&&&), (>>>), arr, returnA)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Data.Function (fix)
import qualified Data.Map as M
import Data.Maybe (isNothing)

import FRP.Yampa (Event(..), SF,
                  isEvent, repeatedly, filterE, tag, rMerge, hold, dAccumHoldBy)

import qualified Graphics.UI.SDL as SDL

import qualified Timer
import SDLUtils (loadImage, setColorKey)
import YampaUtils (reactimate)

screenWidth     = 640
screenHeight    = 480
screenBPP       = 32

framesPerSecond = 30
ticksPerFrame   = 1 / framesPerSecond

wall = SDL.Rect 300 40 40 400

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
           , c_wallColor :: SDL.Pixel
           , c_square    :: SDL.Surface }

squareSide = 20

data Square = Square Int Int

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  white  <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff
  grey   <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0x77 0x77 0x77
  squareSprite <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "square.bmp"

  SDL.setCaption "Move the Square" ""

  let context = GContext screen white grey squareSprite


  timer <- Timer.mkTimer

  let init = Timer.start timer >> return noInput

  let sense = do
        fix $ \loop -> do
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
          Timer.restart timer

          if isNothing input
            then return (time, Just noInput)
            else return (time, input)

  let actuate Nothing _  = return True
      actuate (Just square) beat
        | isEvent beat = renderScreen context square >> return False
        | otherwise    = return False

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (square beat &&& beat)

  SDL.quit

  where arrows = M.fromList [(SDL.SDLK_UP,    UpKey),
                             (SDL.SDLK_DOWN,  DownKey),
                             (SDL.SDLK_LEFT,  LeftKey),
                             (SDL.SDLK_RIGHT, RightKey)]
        handleKeysym keysym = do
          M.lookup (SDL.symKey keysym) arrows

        noInput = Input NoEvent NoEvent False

square :: SF () (Event ()) -> SF Input (Maybe Square)
square beat =
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
        arr fst &&& avoidCollision >>> respectBoundaries -< ((x, y), (vx, vy))

      x <- dAccumHoldBy (+) 0 -< beat' `tag` rvx
      y <- dAccumHoldBy (+) 0 -< beat' `tag` rvy

    returnA -< if closed then Nothing else Just (Square x y)

  where v :: SF (Event a, Event b) Int
        v = proc (activate, deactivate) ->
              hold 0 -< activate `tag` dv
                        `rMerge`
                        deactivate `tag` 0

        dv = squareSide `div` 2

        respectBoundaries :: SF ((Int, Int), (Int, Int)) (Int, Int)
        respectBoundaries =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if breaksBoundaries (x + vx, y) then 0 else vx
            let rvy = if breaksBoundaries (x, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        avoidCollision :: SF ((Int, Int), (Int, Int)) (Int, Int)
        avoidCollision =
          proc ((x, y), (vx, vy)) -> do
            let rvx = if hasWallCollision (x + vx,  y) then 0 else vx
            let rvy = if hasWallCollision (x + rvx, y + vy) then 0 else vy

            returnA -< (rvx, rvy)

        breaksBoundaries :: (Int, Int) -> Bool
        breaksBoundaries (x, y) =
          or [left < 0, top < 0, bottom > screenHeight, right > screenWidth]
          where left   = x
                right  = x + squareSide
                top    = y
                bottom = y + squareSide

        hasWallCollision :: (Int, Int) -> Bool
        hasWallCollision (x, y) = not $ or [bottom <= wallTop,
                                            top    >= wallBottom,
                                            left   >= wallRight,
                                            right  <= wallLeft]
          where wallTop    = SDL.rectY wall
                wallBottom = SDL.rectY wall + SDL.rectH wall
                wallLeft   = SDL.rectX wall
                wallRight  = SDL.rectX wall + SDL.rectW wall

                left       = x
                right      = x + squareSide
                top        = y
                bottom     = y + squareSide

renderScreen :: GContext -> Square -> IO ()
renderScreen (GContext screen white grey square) (Square x y) = do
  fillBg
  drawWall
  drawSquare
  SDL.flip screen

  where fillBg     = SDL.fillRect screen Nothing white
        drawWall   = SDL.fillRect screen (Just wall) grey
        drawSquare =
          SDL.blitSurface square Nothing screen (Just $ SDL.Rect x y 0 0)