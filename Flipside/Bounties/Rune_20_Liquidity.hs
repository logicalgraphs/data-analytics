module Flipside.Bounties.Rune_20_Liquidity where

{--
Of the wallets that have supplied liquidity in THORchain pools, what percentage
have removed at least some of the liquidty?

Hint: use liquidity_actions
--}

import Control.Monad (void)

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Flipside.Data.LPsbyWalletTVL
         (LPbyWalletTVL,lpAdditions,lpSubtractions,wallet_address,tvl_rune)

-- first of all, we want to report on unique addresses that contribute
-- to any LP, then ones that take distributions from LPs

-- The intersection of the two is the set of wallets for this bounty

version0 :: IO ()
version0 =
   putStrLn ("Unique addresses that added or removed liquidity into the "
         ++ "THORchain.") >>
   lpAdditions >>= reportWallets "added to" >>
   lpSubtractions >>= void . reportWallets "removed from"
   

walletsFrom :: [LPbyWalletTVL] -> Set String
walletsFrom = Set.fromList . mapMaybe wallet_address

reportWallets :: String -> [LPbyWalletTVL] -> IO (Set String)
reportWallets action lps = let wal = walletsFrom lps in
   putStrLn ("There are " ++ show (Set.size wal) ++ " unique wallets that "
            ++ action ++ " liquidity pools.") >>
   return wal

-- Version 1: now we intersect the two to find wallets that removed liquidity
-- after having added liquidity

version1 :: IO ()
version1 =
   lpAdditions    >>= \addz ->
   lpSubtractions >>= \subs ->
   let [a, s] = map walletsFrom [addz, subs]
       addedThenRemovedLiq = Set.intersection a s
   in  putStrLn ("Unique wallet addresses that added liquidity then removed "
              ++ "liquidity: " ++ show (length addedThenRemovedLiq))
