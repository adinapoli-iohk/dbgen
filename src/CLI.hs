
module CLI where

import           Data.Maybe
import           Data.Monoid
import           Data.String.Conv
import           Options.Applicative
import           Options.Generic
import           Pos.Wallet.Web.ClientTypes.Types

data CLI = CLI {
    config     :: FilePath
    -- ^ The path to the config file
  , nodePath   :: Maybe FilePath
    -- ^ The path to a valid rocksdb database.
  , walletPath :: Maybe FilePath
    -- ^ The path to a valid acid-state database.
  , addTo      :: Maybe CAccountId
    -- ^ If specified, only append addresses to the
    -- given <wallet_id@account_id>
  , showStats  :: Bool
    -- ^ If true, print the stats for the `wallet-db`
  }

instance ParseRecord CLI where
  parseRecord = CLI <$> fmap (fromMaybe "./config.dhall") (optional (
    strOption (long "config" <> metavar "CONFIG.DHALL"
                             <> help "A path to a Dhall file. (Default: ./config.dhall)"
              )))
              <*> optional (strOption (long "nodeDB" <> metavar "rocksdb-path"
                             <> help "A path to a valid rocksdb database."
                                      ))
              <*> optional (strOption (long "walletDB" <> metavar "acidstate-path"
                             <> help "A path to a valid acidstate database."
                                      ))
              <*> fmap (fmap (CAccountId . toS))
                  (optional (strOption (long "add-to" <> metavar "walletId@accountId"
                             <> help "Append to an existing wallet & account."
              )))
              <*> switch (long "stats" <> help "Show stats for this wallet.")
