{-# LANGUAGE ViewPatterns #-}

module Flipside.Bounties.Rune_19_Whale_Activity where

-- Whales here mean 10k+ RUNE wallets (not RUNE USD, so ... there.)
-- But we can convert RUNE_USD to RUNE, because RUNE is currently ... $12.92

import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord    -- for Down

import Flipside.Data.LPsbyWalletTVL (lps, LpMap)
import Flipside.Data.WalletBalance
          (thorWallets, WalletBalance(WalletBalance))
import Flipside.Reports.Charts.Pie (toPieChart)
import Flipside.Reports.Charts.SQL (chart)
import Flipside.Reports.Utils (printLPs, printList)

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

version2 :: IO ()
version2 = putStrLn "LP-HODLrs\n" >> lps >>= printLPs

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

-- Version 5: ... as a pie chart

version5 :: IO ()
version5 = whalesLPsM >>= toPieChart . whalesPerLP

-- Version 6: ... as a bar chart via SQL

data KVpair = KV String Int

toKV :: (String, Int) -> KVpair
toKV = uncurry KV

instance Univ KVpair where explode (KV a b) = [quote a, show b]

version6 :: IO ()
version6 = whalesLPsM >>= toBarChart . whalesPerLP

toBarChart :: [(String, Int)] -> IO ()
toBarChart = mapM_ putStrLn . chart (words "LP n_whales") . map toKV
