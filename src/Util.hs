module Util (mapAsyncUnordered, timedMs) where

import qualified System.Clock as CS
import Streamly
import qualified Streamly.Prelude as S

mapAsyncUnordered :: Int -> [a] -> (a -> IO ()) -> IO ()
mapAsyncUnordered par collection f =
  runStream
    $ asyncly -- https://stackage.org/haddock/lts-13.13/streamly-0.5.2/Streamly-Tutorial.html#g:2
    $ avgRate 100000 -- https://github.com/composewell/streamly/issues/181
    $ maxThreads par
    $ S.fromFoldableM -- for some reason s.fromList is super slow
    $ fmap f collection

timedMs :: IO a -> IO (a, Double)
timedMs m = do
  start <- getTimeNs
  a <- m
  end <- getTimeNs
  pure (a,  end / 1000000 - start / 1000000)

-- https://www.stackage.org/haddock/lts-12.0/clock-0.7.2/System-Clock.html
getTimeNs :: IO Double
getTimeNs = fmap (fromIntegral . CS.toNanoSecs) (CS.getTime CS.Monotonic)