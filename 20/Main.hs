{-# LANGUAGE Arrows #-}

module Main (main)
       where

import Control.Arrow (returnA,
                      (&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)

import FRP.Yampa (Event (..), SF,
                  isEvent, filterE, tag, rMerge, repeatedly,
                  hold, iPre, dAccumHoldBy)

import qualified Graphics.UI.SDL as SDL

import SDLUtils (loadImage, setColorKey)
import qualified Timer
import YampaUtils (reactimate)

data Sprite =
  Sprite { image  :: SDL.Surface  -- a source image to the sprite
         , frames :: [SDL.Rect] } -- a sequence of frames described by its
                                  -- position at the image

data Direction = MoveLeft
               | MoveRight
               | HoldStillLeft
               | HoldStillRight
               deriving (Show, Eq, Ord)

data ArrowKey = LeftKey
              | RightKey
              deriving Eq

data Input =
  Input { keyDown :: Event ArrowKey
        , keyUp   :: Event ArrowKey
        , close   :: Event () }

data Figure =
  Figure { direction :: Direction
         , position  :: (Int, Int)
         , frame     :: Int }
  deriving Show


screenWidth  = 640
screenHeight = 480
screenBPP    = 32

figureWidth  = 64
figureHeight = 205

-- starting positions
figureX      = 0
figureY      = 275

framesPerSecond = 10
ticksPerFrame   = 1 / framesPerSecond

movingRightFrames    = [SDL.Rect x 0 figureWidth figureHeight
                         | x <- take 4 $ iterate (+figureWidth) 0]
movingLeftFrames     = [SDL.Rect x figureHeight figureWidth figureHeight
                         | x <- take 4 $ iterate (+figureWidth) 0]
holdStillRightFrames = [head movingRightFrames]
holdStillLeftFrames  = [head movingLeftFrames]

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]

  screen <- SDL.setVideoMode screenWidth screenHeight screenBPP [SDL.SWSurface]
  figureImage <- (`setColorKey` (0x00, 0xff, 0xff)) =<< loadImage "foo.png"

  SDL.setCaption "Animation Test" ""

  timer <- Timer.mkTimer

  let movingRightSprite    = Sprite figureImage movingRightFrames
  let movingLeftSprite     = Sprite figureImage movingLeftFrames
  let holdStillRightSprite = Sprite figureImage holdStillRightFrames
  let holdStillLeftSprite  = Sprite figureImage holdStillLeftFrames

  let init  = Timer.start timer >> return noInput

  let sense = do
        event <- SDL.pollEvent

        let input =
              case event of
                SDL.NoEvent        -> Nothing
                SDL.Quit           ->
                  Just $ Input NoEvent NoEvent (Event ())
                SDL.KeyDown keysym ->
                  Just $ Input (handleKeysym keysym) NoEvent NoEvent
                SDL.KeyUp keysym   ->
                  Just $ Input NoEvent (handleKeysym keysym) NoEvent
                _                  -> Nothing

        when (isNothing input) $
          threadDelay 10000

        timeSpent <- fmap ((/1000) . fromIntegral) (Timer.getTicks timer)
        Timer.restart timer

        if isNothing input
          then return (timeSpent, Just noInput)
          else return (timeSpent, input)

        where handledKeysyms =
                M.fromList [ (SDL.SDLK_LEFT,  LeftKey)
                           , (SDL.SDLK_RIGHT, RightKey) ]

              handleKeysym keysym = maybe NoEvent Event mapping
                where mapping = M.lookup (SDL.symKey keysym) handledKeysyms

  let actuate Nothing                              _       = return True
      actuate _                                    NoEvent = return False
      actuate (Just figure@(Figure direction _ _)) _       = do
        renderScreen screen sprite figure
        return False

        where spriteMapping =
                M.fromList [ (HoldStillLeft,  holdStillLeftSprite)
                           , (HoldStillRight, holdStillRightSprite)
                           , (MoveLeft,       movingLeftSprite)
                           , (MoveRight,      movingRightSprite) ]
              sprite        = fromJust $ M.lookup direction spriteMapping

  let beat = repeatedly ticksPerFrame ()

  reactimate init sense (uncurry actuate) (figure beat &&& beat)

  SDL.quit

  where noInput = Input NoEvent NoEvent NoEvent

figure :: SF () (Event ()) -> SF Input (Maybe Figure)
figure beat' =
  proc (Input down up close) -> do
    beat <- beat' -< ()

    let leftD  = filterE (== LeftKey)  down
    let leftU  = filterE (== LeftKey)  up
    let rightD = filterE (== RightKey) down
    let rightU = filterE (== RightKey) up

    left  <- hold False -< flipE leftD  leftU
    right <- hold False -< flipE rightD rightU

    rec
      prevDirection <- delayDirection -< direction
      direction     <- chooseDirection -< (prevDirection, (left, right))

    rec
      vx  <- velocity -< direction
      bvx <- respectBoundaries -< (x, vx)
      x   <- dAccumHoldBy (+) figureX -< beat `tag` bvx

    rec
      let frameSwitch = beat `tag` nextFrame prevFrame prevDirection direction

      prevFrame <- iPre 0 -< frame
      frame     <- hold 0 -< frameSwitch

    returnA -< if isEvent close then
                 Nothing
               else
                 Just $ Figure direction (x, figureY) frame

    where flipE :: Event a -> Event b -> Event Bool
          flipE activate deactivate =
            activate `tag` True
            `rMerge`
            deactivate `tag` False

          delayDirection :: SF Direction Direction
          delayDirection = iPre HoldStillRight

          respectBoundaries :: SF (Int, Int) Int
          respectBoundaries =
            proc (x, v) ->
              returnA -< if breaksBoundaries (x + v) then 0 else v

          breaksBoundaries :: Int -> Bool
          breaksBoundaries x = x < 0 || x + figureWidth > screenWidth

          velocity :: SF Direction Int
          velocity =
            proc direction ->
              returnA -< case direction of
                           MoveLeft  -> -v
                           MoveRight ->  v
                           _         ->  0
            where v = figureWidth `div` 4

          chooseDirection :: SF (Direction, (Bool, Bool)) Direction
          chooseDirection =
            proc x ->
              returnA -< case x of
                           (_, (True, False))  -> MoveLeft
                           (_, (False, True))  -> MoveRight

                           (HoldStillLeft,  _) -> HoldStillLeft
                           (MoveLeft,       _) -> HoldStillLeft
                           (HoldStillRight, _) -> HoldStillRight
                           (MoveRight,      _) -> HoldStillRight

          nextFrame :: Int -> Direction -> Direction -> Int
          nextFrame prev prevDirection direction =
            if direction `elem` [HoldStillLeft, HoldStillRight] then
              0
            else if prevDirection /= direction then
              0
            else
              (prev + 1) `mod` 4

renderScreen :: SDL.Surface -> Sprite -> Figure -> IO ()
renderScreen screen (Sprite image rects) (Figure direction (x, y) frame) = do
  fillBg
  drawSprite

  SDL.flip screen
  where fillBg = do
          white  <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xff 0xff 0xff
          SDL.fillRect screen Nothing white
        drawSprite =
          SDL.blitSurface image (Just $ srcRect) screen (Just $ dstRect)
          where srcRect = rects !! frame
                dstRect = SDL.Rect x y 0 0
