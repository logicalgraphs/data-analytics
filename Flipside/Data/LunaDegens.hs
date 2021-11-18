module Flipside.Data.LunaDegens where

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

-- from my CryptoCoin repository

import Data.CryptoCurrency.Utils (fileProcessor)

-- from 1HaskellADay

import Control.Scan.CSV (readMaybe)

data LunaDegen = LD { address :: String, score :: Int }
   deriving (Eq, Ord, Show)

snarfLunaDegen :: [String] -> Maybe LunaDegen
snarfLunaDegen (_id:addr:score:_whatevs) =
   LD addr <$> readMaybe score
snarfLunaDegen line = error ("Can't read " ++ show line)

degenFile :: FilePath
degenFile = "smol.csv"

readLunaDegens :: FilePath -> IO (Map String LunaDegen)
readLunaDegens file =
   Map.fromList . map (address &&& id) <$> fileProcessor snarfLunaDegen file

{--
>>> degens <- readLunaDegens degenFile
>>> head (Map.toList degens)
("terra100023m27dq557redx2pt7ugnvy5h3mmac8ktlw",
 LD {address = "terra100023m27dq557redx2pt7ugnvy5h3mmac8ktlw", score = 9})

>>> Map.size degens
751076

... that was for the biggie data set. Now let's try it for smol:

>>> degens <- readLunaDegens degenFile
>>> head (Map.toList degens)
("terra100d87dhfca4x023yphu0qfslwnn0vmhgxqpdy3",
 LD {address = "terra100d87dhfca4x023yphu0qfslwnn0vmhgxqpdy3", score = 28})

>>> Map.size degens
6340

TA-DAH!
--}
