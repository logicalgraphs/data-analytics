{-# LANGUAGE OverloadedStrings #-}

module Flipside.Data.LPsbyWalletTVL where

-- we represent Liquidity pools by wallet by (total) volume (locked)

import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)

data LPbyWalletTVL =
   LWT { pool :: String, wallet_address :: Maybe String, tvl_rune :: Double }
      deriving (Eq, Ord, Show)

instance FromJSON LPbyWalletTVL where
   parseJSON = withObject "LPbyWallet" $ \o -> LWT <$> o .: "POOL_NAME"
                     <*> o .:? "FROM_ADDRESS"
                     <*> o .: "TOTAL_RUNE_AMOUNT"

decodeLWTs :: ByteString -> Either String [LPbyWalletTVL]
decodeLWTs = eitherDecode
