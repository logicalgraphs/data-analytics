module Flipside.Bounties.Rune_18_Wealth_Distribution where

-- we read JSON from an endpoint and return rows of data for analytical goodness!

import Control.Arrow (second)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)

-- 1HaskellADay modules

import Control.Presentation (Univ, explode)
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Monetary.USD (doubledown)

-- Flipside modules

import Flipside.Control.Scanner (fetchWith, decodeObjs)
import Flipside.Reports.Charts.SQL (chart)
import Flipside.Reports.WalletBalances (walletReport)
import Flipside.Data.WalletBalance
         (WalletBalance(WalletBalance), address, toWallet, balance,
          summerUrl, parseWallets, thorWallets)

version0 :: IO ()
version0 = fetchWith summerUrl decodeObjs >>=
           putStrLn . ("There are " ++) . (++ " rows of data.") . show . length

{--
>>> version0 
There are 11133 rows of data.
--}

-- Now, let's snarf subtractions from wallets:

version1 :: IO ()
version1 = parseWallets summerUrl >>= walletReport

{--
>>> version1
There were 0 errors in parsing the wallets.

The top 2 wallets are:
thor1g98cy...8n67c0,$376249656.97
thor1dheyc...4qxtxt,$371814281.85

The bottom 2 wallets are:
thor1kf2yv...63tfad,$0.00
thor1tuqft...sj53hf,$0.00

wut. go home, or something.
--}

-- So, now: version 2: we read plusses and minuses and merge them.

version2 :: IO ()
version2 = thorWallets >>= walletReport

-- version 3: groupBy log ... but how do we do that? a list of functions? idk.

logs :: [Double] -> Bag Int
logs = Map.filterWithKey (flip (const (> 0)))
     . Bag.fromList . map (floor . logBase 10)

version3 :: IO ()
version3 = thorWallets >>= mapM_ print . walletDistribution

walletDistribution :: [WalletBalance] -> [(Int, Int)]
walletDistribution =
   map (second getSum) . Map.toList . logs . map (doubledown . balance)

{--
>>> version3
There were a total of 11133 wallets.
There were 0 errors in parsing the wallets.
There were a total of 8434 wallets.
There were 0 errors in parsing the wallets.
(1,2970)
(2,1838)
(3,2047)
(4,814)
(5,238)
(6,40)
(7,6)
(8,2)
--}

-- version 4: LogDistribution (to export as SQL)

data LogDistribution = LD Int Int

instance Univ LogDistribution where explode (LD a b) = map show [a,b]

toLD :: (Int, Int) -> LogDistribution
toLD = LD . fst <*> snd

version4 :: IO ()
version4 =
   thorWallets >>=
       putStrLn
       . unlines
       . chart (words "usd_log10 n_wallets")
       . map toLD
       . walletDistribution
