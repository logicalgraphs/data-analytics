module Flipside.Data.LunaDegens where

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map

import System.Environment (getEnv)

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
degenFile = "articles/bounty-hunts/levana/scores_1_to_30_2021-11-17.csv"

readLunaDegens :: FilePath -> IO (Map String LunaDegen)
readLunaDegens file =
   getEnv "CRYPTOCOIN_DIR" >>= \dir ->
   let degenf = dir ++ ('/':file) in
   Map.fromList . map (address &&& id) <$> fileProcessor snarfLunaDegen degenf

{--
>>> degens <- readLunaDegens degenFile
>>> head (Map.toList degens)
("terra100023m27dq557redx2pt7ugnvy5h3mmac8ktlw",
 LD {address = "terra100023m27dq557redx2pt7ugnvy5h3mmac8ktlw", score = 9})

>>> Map.size degens
751076
--}
