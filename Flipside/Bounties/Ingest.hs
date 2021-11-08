module Flipside.Bounties.Ingest where

-- we read JSON from an endpoint and return rows of data for analytical goodness!

import Control.Arrow ((&&&), second)

import Data.Either (lefts, rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)

-- 1HaskellADay modules

import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Monetary.USD (doubledown)

-- Flipside modules

import Flipside.Control.Scanner (fetchWith, decodeObjs)
import Flipside.Reports.WalletBalances (walletReport)
import Flipside.Data.WalletBalance
         (WalletBalance(WalletBalance), address, toWallet, balance)

summerUrl :: FilePath
summerUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "abe74b5e-884a-4d67-a19f-a5b6fca6038f/data/latest"

version0 :: IO ()
version0 = fetchWith summerUrl decodeObjs >>=
           putStrLn . ("There are " ++) . (++ " rows of data.") . show . length

{--
>>> version0 
There are 11133 rows of data.
--}

-- Now, let's snarf subtractions from wallets:

winterUrl :: FilePath
winterUrl = "https://api.flipsidecrypto.com/api/v2/queries/"
         ++ "7a0de1a8-8ac1-42d6-a52e-0773f6e4e231/data/latest"

version1 :: IO ()
version1 = parseWallets summerUrl >>= walletReport

parseWallets :: FilePath -> IO [WalletBalance]
parseWallets url =
   fetchWith url decodeObjs >>= \rawWallets ->
   let leftandrights = map toWallet rawWallets
       totes = "There were a total of " ++ show (length leftandrights)
            ++ " wallets."
       errs  = "\nThere were " ++ show (length (lefts leftandrights))
            ++ " errors " ++ "in parsing the wallets." in
   putStrLn (totes ++ errs) >>
   return (rights leftandrights)

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

-- Fortunately, we have Map.unionWithKey ... or, actually: Map.unionsWith

walletMap :: FilePath -> IO (Map String WalletBalance)
walletMap url = Map.fromList . map (address &&& id) <$> parseWallets url

version2 :: IO ()
version2 = thorWallets >>= walletReport

thorWallets :: IO [WalletBalance]
thorWallets = 
   walletMap summerUrl >>= \plusses ->
   walletMap winterUrl >>= \minuses ->
   let wallets = Map.unionsWith (-) [plusses, minuses]
   in  return (Map.elems wallets)

-- version 3: groupBy log ... but how do we do that? a list of functions? idk.

logs :: [Double] -> Bag Int
logs = Map.filterWithKey (flip (const (> 0)))
     . Bag.fromList . map (floor . logBase 10)

version3 :: IO ()
version3 =
   thorWallets >>= 
   mapM_ (print . second getSum)
         . Map.toList . logs . map (doubledown . balance)

{--
>>> wals <- thorWallets 
There were a total of 11133 wallets.
There were 0 errors in parsing the wallets.
There were a total of 8434 wallets.
There were 0 errors in parsing the wallets.

>>> let chart = logs (map (doubledown . balance) wals)
>>> mapM_ (print . second getSum) (Map.toList chart)
(1,2970)
(2,1838)
(3,2047)
(4,814)
(5,238)
(6,40)
(7,6)
(8,2)
--}
