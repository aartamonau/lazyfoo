module YampaUtils (reactimate, flipE)
       where

import FRP.Yampa (SF, DTime, Event,
                  tag, rMerge)
import qualified FRP.Yampa (reactimate)

reactimate :: IO a -> IO (DTime, Maybe a) -> (b -> IO Bool) -> SF a b -> IO ()
reactimate init sense actuate sf =
  FRP.Yampa.reactimate init (const sense) (const actuate) sf

flipE :: Event a -> Event b -> Event Bool
flipE activate deactivate =
  activate `tag` True
  `rMerge`
  deactivate `tag` False