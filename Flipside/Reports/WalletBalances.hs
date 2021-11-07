{-# LANGUAGE ViewPatterns #-}

module Flipside.Reports.WalletBalances where

import Data.List (sortOn)
import Data.Ord    -- for Down

import Flipside.Data.WalletBalance (WalletBalance(WalletBalance), balance)

-- 1HaskellADay modules

import Control.List (weave)

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

-- a little non-doxing here:

masqueAddr :: String -> String
masqueAddr s = take 10 s ++ "..." ++ drop 37 s
