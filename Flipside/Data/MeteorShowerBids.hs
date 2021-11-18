{-# LANGUAGE OverloadedStrings #-}

module Flipside.Data.MeteorShowerBids where

-- we represent Liquidity pools by wallet by (total) volume (locked)

import Control.Arrow ((&&&), (***))
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Flipside.Control.Scanner (fetchWith)

-- from 1HaskellADay

import Data.Monetary.USD

data MeteorShowerBid =
   MSB { meteorologist :: String, bids :: Integer, bidTotal :: USD }
      deriving (Eq, Ord, Show)

instance FromJSON MeteorShowerBid where
   parseJSON = withObject "MeteorShowerBid" $ \o ->
      MSB <$> o .: "METEOROLOGIST"
          <*> o .: "N_BIDS"
          <*> (o .: "TOTAL_BID_UST" >>= return . USD . fromRational)

decodeMSBs :: ByteString -> Either String [MeteorShowerBid]
decodeMSBs = eitherDecode

showerUrl :: FilePath
showerUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "5de2b143-48ba-4645-89c0-76b0fbd0241d/data/latest"

lpMeteors :: IO [MeteorShowerBid]
lpMeteors = fetchWith showerUrl decodeMSBs

lps :: IO (Map String MeteorShowerBid)
lps = Map.fromList . map (meteorologist &&& id) <$> lpMeteors
