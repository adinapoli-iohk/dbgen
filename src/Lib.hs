{-# LANGUAGE DeriveGeneric #-}
module Lib where

import GHC.Generics (Generic)
import Dhall
import Data.String.Conv

data GenSpec = GenSpec {
  wallets  :: !Integer
  -- ^ How many wallets to create
  , wallet_spec :: WalletSpec
  -- ^ The specification for each wallet.
  } deriving (Show, Eq, Generic)

instance Interpret GenSpec

data WalletSpec = WalletSpec {
  accounts      :: !Integer
  -- ^ How many accounts to generate
  , account_spec :: AccountSpec
  -- ^ How specification for each account.
  } deriving (Show, Eq, Generic)

instance Interpret WalletSpec

data AccountSpec = AccountSpec {
  addresses :: !Integer
  -- How many addresses to generate.
  } deriving (Show, Eq, Generic)

instance Interpret AccountSpec

loadGenSpec :: FilePath -> IO GenSpec
loadGenSpec = input auto . toS

generate :: GenSpec -> IO ()
generate GenSpec{..} = return ()
