module Flipside.Bounties.Levana_2_Meteors where

{--
For those who bought and participated in the Levana Meteor Shower, is there a 
relationship between their participation and their LUNATIC degen score?
--}

import Control.Monad (void)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Flipside.Data.MeteorShowerBids (lps, MeteorShowerBid, meteorologist)
import Flipside.Data.LunaDegens (readLunaDegens, LunaDegen, score, degenFile)

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
