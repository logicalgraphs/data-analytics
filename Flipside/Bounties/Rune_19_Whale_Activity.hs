module Flipside.Bounties.Rune_19_Whale_Activity where

-- Whales here mean 10k+ RUNE wallets (not RUNE USD, so ... there.)
-- But we can convert RUNE_USD to RUNE, because RUNE is currently ... $12.92

import Flipside.Data.WalletBalance (thorWallets, WalletBalance(WalletBalance))

-- 1HaskellADay modules:

import Data.Monetary.USD (USD(USD))

data RuneWallet = RuneWallet { addr :: String, balance :: Double }
   deriving (Eq, Ord, Show)

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
