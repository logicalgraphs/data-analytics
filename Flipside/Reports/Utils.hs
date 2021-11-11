module Flipside.Reports.Utils where

-- some helper functions for managing (too much) data in reports

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord    -- for Down

printLPs :: Show k => Show a => Ord a => Map k a -> IO ()
printLPs = printList . sortOn (Down . snd) . Map.toList

printList :: Show a => [a] -> IO ()
printList lps =
   let len = length lps
       frist = map show (take 2 lps)
       lest =  map show (drop (len - 2) lps) in
   mapM_ putStrLn (frist ++ ("...":lest))
