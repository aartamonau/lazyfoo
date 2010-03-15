module YampaUtils (reactimate)
       where

import FRP.Yampa (SF, DTime)
import qualified FRP.Yampa (reactimate)

reactimate :: IO a -> IO (DTime, Maybe a) -> (b -> IO Bool) -> SF a b -> IO ()
reactimate init sense actuate sf =
  FRP.Yampa.reactimate init (const sense) (const actuate) sf
