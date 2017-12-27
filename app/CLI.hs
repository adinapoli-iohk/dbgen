
module CLI where

import           Data.Maybe
import           Data.Monoid
import           Options.Applicative
import           Options.Generic

data CLI = CLI {
    config :: FilePath
    -- ^ The path to the config file
  }

instance ParseRecord CLI where
  parseRecord = CLI . fromMaybe "./config.dhall" <$> optional (
    strOption (long "config" <> metavar "CONFIG.DHALL"
                             <> help "A path to a Dhall file. (Default: ./config.dhall)"
              ))
