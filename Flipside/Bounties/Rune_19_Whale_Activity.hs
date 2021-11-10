{-# LANGUAGE ViewPatterns #-}

module Flipside.Bounties.Rune_19_Whale_Activity where

-- Whales here mean 10k+ RUNE wallets (not RUNE USD, so ... there.)
-- But we can convert RUNE_USD to RUNE, because RUNE is currently ... $12.92

import Control.Arrow ((&&&), (***))

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord   -- for Down
import qualified Data.Set as Set

import Flipside.Control.Scanner (fetchWith)
import Flipside.Data.LPsbyWalletTVL
          (LPbyWalletTVL, decodeLWTs, pool, wallet_address, tvl_rune)
import Flipside.Data.WalletBalance
          (thorWallets, WalletBalance(WalletBalance))
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

-- Version 2: we go FAR a-field to map wallets to LPs

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

So:
--}

version2 :: IO ()
version2 = putStrLn "LP-HODLrs\n" >> lps >>= printLPs

printLPs :: Show a => Ord a => LpMap a -> IO ()
printLPs = printList . sortOn (Down . snd) . Map.toList

printList :: Show a => [a] -> IO ()
printList lps =
   let len = length lps
       frist = map show (take 2 lps)
       lest =  map show (drop (len - 2) lps) in
   mapM_ putStrLn (frist ++ ("...":lest))

{--
>>> version2
LP-HODLrs

(("ETH.THOR-0","thor1wr4jeacpv77vrcfh6kzncnpx3tgrkazwljrwx3"),351500.0)
(("ETH.THOR-0","thor1s6rjvqmwwx9wgvq9sztcsuptl48d0qch0vadkk"),200073.91)
...
(("ETH.THOR-0","thor14xqzqtzymwhfhckk62e3yxz3kr6war0cmp5l2y"),1.01453594)
(("BNB.BNB","thor1lxy5y7sc6sc4scc4dpz0jvgcf900hq94et2v3u"),1.00541054)
--}

-- Version 3: Filter out some non-whales.

whaleLPs :: [RuneWallet] -> LpMap a -> LpMap a
whaleLPs (Set.fromList . map addr -> whales) =
   Map.filterWithKey (\k _v -> Set.member (snd k) whales)

whalesLPsM :: IO (LpMap Double)
whalesLPsM = whalesM >>= \w -> lps >>= return . whaleLPs w

version3 :: IO ()
version3 = whalesLPsM >>= printLPs

{--
>>> version3
There were a total of 11133 wallets.
There were 0 errors in parsing the wallets.
There were a total of 8434 wallets.
There were 0 errors in parsing the wallets.
(("ETH.THOR-0","thor1wr4jeacpv77vrcfh6kzncnpx3tgrkazwljrwx3"),351500.0)
(("ETH.ETH","thor1wr4jeacpv77vrcfh6kzncnpx3tgrkazwljrwx3"),148410.0)
...
(("BTC.BTC","thor1dh4xjwerzv04gxzh63xhteu5cpjk7zhttcqxc0"),10.0)
(("ETH.ETH","thor1p3vydy9ryeykh05x69j0zvh5x07hx27wdp3xaq"),6.85046631)
--}

-- Now we report out (whale) wallets by LP, which is the key of LpMap

whalesPerLP :: LpMap a -> [(String, Int)]
whalesPerLP =
   sortOn (Down . snd) . Map.toList . Map.map length . snarfL Just . Map.keys

version4 :: IO ()
version4 = whalesLPsM >>= printList . whalesPerLP

{--
>>> version4
There were a total of 11133 wallets.
There were 0 errors in parsing the wallets.
There were a total of 8434 wallets.
There were 0 errors in parsing the wallets.
("ETH.THOR-0",44)
("BTC.BTC",24)
...
("ETH.USDT-0",1)
("ETH.YFI-0X",1)
--}
