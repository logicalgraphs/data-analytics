{-# LANGUAGE ViewPatterns #-}

module Flipside.Bounties.Ingest where

-- we read JSON from an endpoint and return rows of data for analytical goodness!

import Control.Arrow ((&&&))

import Data.Aeson (eitherDecode, FromJSON, Value, fromJSON, Result(Success))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (lefts, rights)
import Data.List (partition, isSuffixOf, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord    -- for Down

import Network.HTTP.Conduit (simpleHttp)

-- 1HaskellADay modules:

import Control.List (weave)

import Data.Monetary.USD

fetchWith :: FromJSON a => FilePath -> (ByteString -> Either String [a])
                        -> IO [a]
fetchWith url f = simpleHttp url >>= reportErrors . f

reportErrors :: Either String [a] -> IO [a]
reportErrors (Left err) = error err
reportErrors (Right ans) = return ans

-- a bare-level (pass-thru) parser

decodeObjs :: ByteString -> Either String [Map String Value]
decodeObjs = eitherDecode

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

data WalletBalance = WalletBalance { address :: String, balance :: USD }
   deriving (Eq, Ord, Show)

-- toWallet is interesting, because you have foo_address, then ... something
-- else. How do we parse this in from the JSON thusly structured?

type KV = (String, Value)
type Spread = ([KV], [KV])

scanner :: Map String Value -> Spread
scanner = partition (("ADDRESS" `isSuffixOf`) . fst) . Map.toList

toWallet :: Map String Value -> Either String WalletBalance
toWallet = toWallet' . scanner

{--
>>> wals <- fetchWith summerUrl decodeObjs 
>>> scanner (head wals)
([("TO_ADDRESS",String "thor1lcxekvew23dju4jf7hnzhua7y0vkdx48v5sxkp")],
 [("PLUS",Number 2.0986114449295286e7)])

So, now we need to convert this to a wallet. Which, because types, is easy! AHA!
--}

toWallet' :: Spread -> Either String WalletBalance
toWallet' ([(_addrKey, addr)], [(_balKey, num)]) = 
   toWal'' (fromJSON addr) (fromJSON num)

toWal'' :: Result String -> Result Double -> Either String WalletBalance
toWal'' (Success addr) (Success bal) =
   Right (WalletBalance addr (USD $ toRational bal))
toWal'' a (Success _) = Left ("Could not read wallet address " ++ show a)
toWal'' (Success _) b = Left ("Could not read wallet balance " ++ show b)
toWal'' a b = Left ("Could not read anything from wallet " ++ show a ++ show b)

{--
>>> toWallet (head wals)
Right (WalletBalance {address = "thor1lcxekvew23dju4jf7hnzhua7y0vkdx48v5sxkp", 
                      balance = $20986114.44})
--}

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

walletReport :: [WalletBalance] -> IO ()
walletReport (sortOn (Down . balance) -> wals) =
   reportWallets "top" wals               >>
   reportWallets "bottom" (reverse wals)  >>
   putStrLn "\nwut. go home, or something."

reportWallets :: String -> [WalletBalance] -> IO ()
reportWallets extrem wals =
   putStrLn ("\nThe " ++ extrem ++ " 2 wallets are:") >>
   mapM_ printWallet (take 2 wals)

printWallet :: WalletBalance -> IO ()
printWallet (WalletBalance addr val) =
   putStrLn (weave [masqueAddr addr, show val])

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

-- a little non-doxing here:

masqueAddr :: String -> String
masqueAddr s = take 10 s ++ "..." ++ drop 37 s

-- So, now: version 2: we read plusses and minuses and merge them.

-- Fortunately, we have Map.unionWithKey

walletMap :: FilePath -> IO (Map String WalletBalance)
walletMap url = Map.fromList . map (address &&& id) <$> parseWallets url

-- hehehehe ... I'm 'bout ta do summthin EEVEEELL! ;)

instance Num WalletBalance where
   w1 + w2  = WalletBalance (address w1) (balance w1 + balance w2)
   w1 * w2  = undefined
   abs w    = w { balance = abs (balance w) }
   signum w = w { balance = signum (balance w) }
   fromInteger a = WalletBalance "foo" (USD (toRational a))
   negate w = w { balance = negate (balance w) }

version2 :: IO ()
version2 = 
   walletMap summerUrl >>= \plusses ->
   walletMap winterUrl >>= \minuses ->
   let wallets = Map.unionsWith (-) [plusses, minuses]
   in  walletReport (Map.elems wallets)
