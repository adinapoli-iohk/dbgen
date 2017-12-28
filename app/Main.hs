{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module Main where

import           CLI
import           Control.Monad.Reader
import           Data.Default                (def)
import           GHC.Conc
import           Lib
import           Mockable                    (Production, runProduction)
import           Options.Generic
import           Pos.Launcher                (withConfigurations)
import           Pos.Launcher.Configuration
import           Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.State  (WalletState)
import           System.Wlog.LoggerName
import           System.Wlog.LoggerNameBox


walletRunner :: HasConfigurations => WalletWebModeContext -> UberMonad a -> IO a
walletRunner wwmc act = runProduction $
    runReaderT act wwmc

newWalletState :: HasConfigurations => IO WalletState
newWalletState = openState True "./wallet-db"

defLoggerName :: LoggerName
defLoggerName = LoggerName "dbgen"

instance HasLoggerName IO where
  getLoggerName = pure defLoggerName
  modifyLoggerName _ x = x

newConfig :: ConfigurationOptions
newConfig = defaultConfigurationOptions {
  cfoSystemStart = Just 0
  }

main :: IO ()
main = withConfigurations newConfig $ do
  cli@CLI{..} <- getRecord "DBGen"
  spec <- loadGenSpec config
  ctx <- WalletWebModeContext <$> newWalletState
                              <*> newTVarIO def
                              <*> return (error "testRealModeContext is currently unimplemented")
  walletRunner ctx (generate spec)
