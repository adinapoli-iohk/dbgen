module Main where

import           CLI
import           Lib
import           Options.Generic

main :: IO ()
main = do
  cli@CLI{..} <- getRecord "DBGen"
  spec <- loadGenSpec config
  generate spec
