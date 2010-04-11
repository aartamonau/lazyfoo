{-# LANGUAGE Arrows #-}

module ArrowUtils (sequenceA)
       where

import Control.Arrow (Arrow, arr, returnA)

sequenceA :: Arrow arr => [arr a b] -> arr a [b]
sequenceA arrs = foldr k (arr $ const []) arrs
  where k arr arr' =
          proc a -> do
            x  <- arr  -< a
            xs <- arr' -< a

            returnA -< (x : xs)
