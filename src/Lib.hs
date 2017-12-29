{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import           Control.Concurrent
import           Control.Lens                   (view)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString                as B
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text                      as T
import           Data.Time
import           Dhall
import           GHC.Generics                   (Generic)
import           Network.Haskoin.Crypto
import           Pos.DB.GState.Common           (getTip)
import           Pos.StateLock
import           Pos.Util.BackupPhrase
import           Pos.Util.Util                  (HasLens', lensOf)
import           Pos.Wallet.Web.ClientTypes
import           Pos.Wallet.Web.Methods.Restore
import           Pos.Wallet.Web.Mode
import qualified Test.QuickCheck                as QC
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
  before <- liftIO getCurrentTime
  res <- action
  after <- liftIO getCurrentTime
  -- NOTE: Mind the `fromEnum` overflow.
  let diff = fromEnum $ after `diffUTCTime` before
  liftIO $ putStrLn $ printf "Action took %f seconds." (fromIntegral diff / (10^12 :: Double))
  return res

type UberMonad a = MonadWalletWebMode WalletWebMode => WalletWebMode a

-- | Log on stdout.
say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

fakeSync :: UberMonad ()
fakeSync = do
  say "Faking StateLock syncing..."
  tip <- getTip
  (StateLock mvar _) <- view (lensOf @StateLock)
  () <$ liftIO (tryPutMVar mvar tip)

-- | The main entry point.
generate :: GenSpec -> UberMonad ()
generate spec@GenSpec{..} = do
  fakeSync
  say $ printf "Generating %d wallets..." wallets
  wallets <- timed (forM [1..wallets] genWallet)
  return ()

-- | Creates a new 'CWallet'.
genWallet :: Integer -> UberMonad CWallet
genWallet walletNum = do
  mnemonic <- newRandomMnemonic (toEntropy walletNum)
  r <- newWallet mempty (walletInit mnemonic)
  return r
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

-- | Generates some 'Entropy' from an 'Integer'. Due to the fact
-- Haskoin accepts only multiple of 4 bytes, we pad the input with
-- zeros. It leads to quite crappy 12-words mnemonic, but as long as
-- they don't clash with each other, we are happy.
toEntropy :: Integer -> Entropy
toEntropy x =
  let packs = B.unpack (toS $ show x)
      len   = length packs
  in B.pack $ case len < 16 of
    True  -> packs <> replicate (16 - len) 0x0
    False -> take (len - (len `mod` 16)) packs

-- | Generates a new 'BackupPhrase' piggybacking on @Haskoin@ for
-- the BIP-39 words list.
newRandomMnemonic :: MonadIO m => Entropy -> m BackupPhrase
newRandomMnemonic entropy = case toMnemonic entropy of
  Left e  -> error ("Error: " <> e)
  Right x -> pure (mkBackupPhrase12 (T.words $ toS x))
