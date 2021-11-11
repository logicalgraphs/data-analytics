{-# LANGUAGE OverloadedStrings #-}

module Flipside.Data.LPsbyWalletTVL where

-- we represent Liquidity pools by wallet by (total) volume (locked)

import Control.Arrow ((&&&), (***))
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Flipside.Control.Scanner (fetchWith)

data LPbyWalletTVL =
   LWT { pool :: String, wallet_address :: Maybe String, tvl_rune :: Double }
      deriving (Eq, Ord, Show)

instance FromJSON LPbyWalletTVL where
   parseJSON = withObject "LPbyWallet" $ \o -> LWT <$> o .: "POOL_NAME"
                     <*> o .:? "FROM_ADDRESS"
                     <*> o .: "TOTAL_RUNE_AMOUNT"

decodeLWTs :: ByteString -> Either String [LPbyWalletTVL]
decodeLWTs = eitherDecode

springUrl, fallUrl :: FilePath
springUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "23cfd730-c60a-45bf-9bc4-cde2a10e23a3/data/latest"
fallUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "6a4d0d06-cbce-431f-9798-bcf818578a48/data/latest"

type RawLpMapKey = (String, Maybe String)
type LpMapKey = (String, String)
type LpMapEntry a = (LpMapKey, a)
type LpMap a = Map LpMapKey a

lpAdditions, lpSubtractions :: IO [LPbyWalletTVL]
lpAdditions = fetchWith springUrl decodeLWTs
lpSubtractions = fetchWith fallUrl decodeLWTs

lps :: IO (LpMap Double)
lps = lpAdditions >>= \adderal ->
      lpSubtractions >>= \nadaral ->
      let maps = map (Map.fromList . lp2Map) [adderal, nadaral]
          mergedMap = Map.unionsWith (-) maps
          withWallets = Map.filterWithKey (\k _v -> snd k /= Nothing) mergedMap 
          realLps = Map.mapKeys (take 10 *** fromJust) withWallets
      in  return (Map.filter (> 1) realLps)

lp2Map :: [LPbyWalletTVL] -> [(RawLpMapKey, Double)]
lp2Map = map ((pool &&& wallet_address) &&& tvl_rune)

{--
>>> ls <- lps
>>> length ls
7559

>>> head $ Map.toList ls 
(("BCH.BCH",Nothing),0.0)

whoops, let's filter out the nothings ... *rewrite, rewrite, rewrite

So, now:

>>> head $ Map.toList ls
(("BCH.BCH","qp38elk9pawlanlfjlxladdy62nspce2rsx2mf9j0q"),0.0)

... is every balance zero?

>>> let bals = Map.filter (> 0) ls
>>> head $ Map.toList bals
(("BCH.BCH","thor10ma5hwscwm0hv3qm4h4ny3gnf0z4gc8zatk2f4"),36.44368104)

Nupe! Good! ... let's at that to the lps returned

>>> ls <- lps
>>> length ls
6036
--}
