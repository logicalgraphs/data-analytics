module Flipside.Bounties.Levana_2_Meteors where

{--
For those who bought and participated in the Levana Meteor Shower, is there a 
relationship between their participation and their LUNATIC degen score?
--}

import Control.Arrow ((&&&))
import Control.Monad (void)

import Data.List (minimum, maximum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Flipside.Data.MeteorShowerBids
             (lps, MeteorShowerBid, meteorologist, bids, bidTotal)
import Flipside.Data.LunaDegens
             (readLunaDegens, LunaDegen, score, degenFile)
import Flipside.Reports.Charts.Bar (toBarChart)

-- 1HaskellADay modules

import Control.Map (snarfL)
import Data.Monetary.USD (USD, doubledown)

-- version 0: first of all, we want to merge the luna degen scores into the 
-- meteor shower participants

data MeteorDegen = MD { metes :: MeteorShowerBid, degen :: Int }
   deriving (Eq, Ord, Show)

meteorDegens :: IO (Map String MeteorDegen)
meteorDegens = lps                      >>= \mets ->
               readLunaDegens degenFile >>=
               return . flip Map.mapMaybe mets . ms2md

ms2md :: Map String LunaDegen -> MeteorShowerBid -> Maybe MeteorDegen
ms2md gens m = MD m . score <$> Map.lookup (meteorologist m) gens

version0 :: IO ()
version0 =
   meteorDegens >>=
   putStrLn . ("There are " ++) . (++ " Luna degen meteor shower participants.")
            . show . length

{--
>>> version0
There are 6340 Luna degen meteor shower participants.
--}

-- version 1: so, what's the average degen score?

version1 :: IO ()
version1 =
   meteorDegens >>= \mets ->
   putStrLn ("The average degen score of a meteor participant is: "
          ++ show (sum (map degen (Map.elems mets)) `div` Map.size mets))

{--
>>> version1
The average degen score of a meteor participant is: 15
--}

-- version2 (removed) ensmolified the 78 MByte Luna degen file to 6000 rows
-- (see the revision history for source code, if'n you're jonesin')

-- Version 3: correlation of degen score to number of bids

snarfDegen :: Ord a => (MeteorShowerBid -> a) -> Map c MeteorDegen -> Map a [Int]
snarfDegen f = snarfL (\m -> Just (f (metes m), degen m)) . Map.elems

barMeteorDegens :: Show a => Ord a => (MeteorShowerBid -> a) -> IO ()
barMeteorDegens f =
   meteorDegens >>= \mets ->
   let sn = snarfDegen f mets
       ns = Map.map avg sn
       an = Map.mapKeys show ns
   in  toBarChart (Map.toList an)

avg :: [Int] -> Int
avg x = sum x `div` length x

version3 :: IO ()
version3 = barMeteorDegens bids

{--
>>> version3
alphabet=[{letter: '1', frequency: 15},
          {letter: '10', frequency: 15},
          {letter: '11', frequency: 15},
          {letter: '12', frequency: 16},...,
          {letter: '8', frequency: 17},
          {letter: '9', frequency: 11},
          {letter: '90', frequency: 23}]
--}

-- version 4: let's find the bid range of our meteor bidder domain

version4 :: IO ()
version4 =
   meteorDegens >>=
   putStrLn . ("The min bid and max bid are: " ++) . show . minimax . Map.elems

minimax :: [MeteorDegen] -> (USD, USD)
minimax = (minimum &&& maximum) . map (bidTotal . metes)

{--
>>> version4
The min bid and max bid are: ($8.00,$74195.00)
--}

-- version 5: let's look at total bids (logarithmic scale) to degen score

version5 :: IO ()
version5 = barMeteorDegens (floor . logBase 10 . doubledown . bidTotal)

{--
>>> version5
alphabet=[{letter: '0', frequency: 14},
          {letter: '1', frequency: 15},
          {letter: '2', frequency: 15},
          {letter: '3', frequency: 17},
          {letter: '4', frequency: 19}]
--}
