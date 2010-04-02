module Utils ( takeWhileM )
       where

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM p = foldr (liftM2' (:) []) (return [])
  where liftM2' f z mx my = do
          x <- mx
          if p x
            then do
              y <- my
              return $ f x y
            else
              return z