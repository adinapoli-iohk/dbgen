
module CLI where

import           Data.Maybe
import           Data.Monoid
import           Options.Applicative
import           Options.Generic

data CLI = CLI {
    config :: FilePath
    -- ^ The path to the config file
  , dbPath :: Maybe FilePath
  }

instance ParseRecord CLI where
  parseRecord = CLI <$> fmap (fromMaybe "./config.dhall") (optional (
    strOption (long "config" <> metavar "CONFIG.DHALL"
                             <> help "A path to a Dhall file. (Default: ./config.dhall)"
              )))
              <*> optional (strOption (long "dbPath" <> metavar "rocksdb-path"
                             <> help "A path to a valid rocksdb."
              ))
