module Flipside.Bounties.Rune_19_Whale_Activity where

-- Whales here mean 10k+ RUNE wallets (not RUNE USD, so ... there.)
-- But we can convert RUNE_USD to RUNE, because RUNE is currently ... $12.92

import Control.Arrow ((&&&), second)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Flipside.Control.Scanner (fetchWith)
import Flipside.Data.LPsbyWalletTVL
          (LPbyWalletTVL, decodeLWTs, pool, wallet_address, tvl_rune)
import Flipside.Data.WalletBalance (thorWallets, WalletBalance(WalletBalance))
import Flipside.Reports.Charts.SQL (chart)

-- 1HaskellADay modules:

import Control.Map (snarfL)
import Control.Presentation (Univ, explode)

import Data.Monetary.USD (USD(USD))

data RuneWallet = RuneWallet { addr :: String, balance :: Double }
   deriving (Eq, Ord, Show)

-- Version 0: let's find out how many whales are in these THORchain seas

toRuneWallet :: WalletBalance -> RuneWallet
toRuneWallet (WalletBalance a (USD d)) = RuneWallet a ((fromRational d) / 12.92)

whales :: [WalletBalance] -> [RuneWallet]
whales = filter ((> 10000) . balance) . map toRuneWallet

whalesM :: IO [RuneWallet]
whalesM = whales <$> thorWallets

version0 :: IO ()
version0 = thorWallets >>= \wals ->
   let ss = show . length
       whals = whales wals in
   putStrLn ("There are " ++ ss whals ++ " whales amongst "
                     ++ ss wals ++ " THORchain wallets.")

-- Version 1: let's extract the addresses as SQL.

instance Univ RuneWallet where explode (RuneWallet a b) = [quote a, show b]

quote :: String -> String
quote = ('\'':) . (++ "'")

version1 :: IO ()
version1 = whalesM >>=
           putStrLn . unlines . chart (words "whale_address rune_balance")

springUrl, fallUrl :: FilePath
springUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "23cfd730-c60a-45bf-9bc4-cde2a10e23a3/data/latest"
fallUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "6a4d0d06-cbce-431f-9798-bcf818578a48/data/latest"

type RawLpMapKey = (String, Maybe String)
type LpMapKey = (String, String)
type LpMapEntry a = (LpMapKey, a)
type LpMap a = Map LpMapKey a

lps :: IO (LpMap Double)
lps = fetchWith springUrl decodeLWTs >>= \adderal ->
      fetchWith fallUrl   decodeLWTs >>= \nadaral ->
      let maps = map (Map.fromList . lp2Map) [adderal, nadaral]
          mergedMap = Map.unionsWith (-) maps
          withWallets = Map.filterWithKey (\k _v -> snd k /= Nothing) mergedMap 
          realLps = Map.mapKeys (second fromJust) withWallets
      in  return (Map.filter (> 0) realLps)

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
