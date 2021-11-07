module Flipside.Data.WalletBalance where

-- We declare the Wallet-type

import Data.Aeson (Value, Result(Success), fromJSON)
import Data.List (partition, isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

-- 1HaskellADay modules:

import Data.Monetary.USD

data WalletBalance = WalletBalance { address :: String, balance :: USD }
   deriving (Eq, Ord, Show)

-- toWallet is interesting, because you have foo_address, then ... something
-- else. How do we parse this in from the JSON thusly structured?

type KV = (String, Value)
type Spread = ([KV], [KV])

scanner :: Map String Value -> Spread
scanner = partition (("ADDRESS" `isSuffixOf`) . fst) . Map.toList

{--
>>> wals <- fetchWith summerUrl decodeObjs
>>> scanner (head wals)
([("TO_ADDRESS",String "thor1lcxekvew23dju4jf7hnzhua7y0vkdx48v5sxkp")],
 [("PLUS",Number 2.0986114449295286e7)])

So, now we need to convert this to a wallet. Which, because types, is easy! AHA!
--}

toWallet :: Map String Value -> Either String WalletBalance
toWallet = toWallet' . scanner

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

-- hehehehe ... I'm 'bout ta do summthin EEVEEELL! ;)

instance Num WalletBalance where
   w1 + w2  = WalletBalance (address w1) (balance w1 + balance w2)
   w1 * w2  = undefined
   abs w    = w { balance = abs (balance w) }
   signum w = w { balance = signum (balance w) }
   fromInteger a = WalletBalance "foo" (USD (toRational a))
   negate w = w { balance = negate (balance w) }
