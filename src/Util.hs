module Util (parallelChunking) where

import qualified Control.Concurrent.Async as CA
import Data.List.Split as LS

-- sequential processing of chunks done in //
-- TODO replace with true streaming
parallelChunking :: Int -> [a] -> (a -> IO ()) -> IO ()
parallelChunking chunkSize collection f =
  mapM_ (
    CA.mapConcurrently_ f
    ) chunkedCollection
  where chunkedCollection = LS.chunksOf chunkSize collection