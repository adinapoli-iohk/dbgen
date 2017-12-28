{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module Lib where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.String.Conv
import           Data.Time.Clock.POSIX
import           Dhall
import           GHC.Generics                   (Generic)
import           Network.Haskoin.Crypto
import           Pos.Util.BackupPhrase
import           Pos.Wallet.Web.ClientTypes
import           Pos.Wallet.Web.Methods.Restore
import           Pos.Wallet.Web.Mode
import           Text.Printf

--
-- Types
--

data GenSpec = GenSpec {
  wallets       :: !Integer
  -- ^ How many wallets to create
  , wallet_spec :: WalletSpec
  -- ^ The specification for each wallet.
  } deriving (Show, Eq, Generic)

instance Interpret GenSpec

data WalletSpec = WalletSpec {
  accounts       :: !Integer
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

--
-- Functions
--

-- | Load the 'GenSpec' from an input file.
loadGenSpec :: FilePath -> IO GenSpec
loadGenSpec = input auto . toS

-- | Run an action and report the time it took.
timed :: MonadIO m => m a -> m a
timed action = do
  before <- liftIO getPOSIXTime
  res <- action
  after <- liftIO getPOSIXTime
  -- NOTE: Mind the `fromEnum` overflow.
  liftIO $ putStrLn $ printf "Action took %s seconds." (fromEnum $ after - before)
  return res

type UberMonad a = MonadWalletWebMode WalletWebMode => WalletWebMode a

-- | The main entry point.
generate :: GenSpec -> UberMonad ()
generate spec@GenSpec{..} = do
  liftIO $ putStrLn $ printf "Generating %d wallets..." wallets
  wallets <- timed (forM [1..wallets] genWallet)
  return ()

-- | Creates a new 'CWallet'.
genWallet :: Integer -> UberMonad CWallet
genWallet walletNum = do
  mnemonic <- newRandomMnemonic (toS $ T.pack . show $ walletNum)
  newWallet mempty (walletInit mnemonic)
  where
    walletInit :: BackupPhrase -> CWalletInit
    walletInit backupPhrase = CWalletInit {
      cwInitMeta      = CWalletMeta {
          cwName      = toS $ "Wallet #" <> show walletNum
        , cwAssurance = CWANormal
        , cwUnit      = 0
        }
    , cwBackupPhrase  = backupPhrase
      }

newRandomMnemonic :: MonadIO m => Entropy -> m BackupPhrase
newRandomMnemonic entropy = case toMnemonic entropy of
  Left e  -> error e
  Right x -> pure (mkBackupPhrase12 (T.words $ toS x))
