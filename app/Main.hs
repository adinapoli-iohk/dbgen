{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           CLI
import           Control.Lens
import           Control.Monad.Reader
import           Data.Default                (def)
import           Data.Maybe                  (fromJust, fromMaybe)
import           GHC.Conc
import           Lib
import           Mockable                    (Production, runProduction)
import           Options.Generic
import           Pos.Client.CLI              (CommonArgs (..), CommonNodeArgs (..),
                                              NodeArgs (..), getNodeParams, gtSscParams)
import           Pos.DB.Rocks.Functions
import           Pos.DB.Rocks.Types
import           Pos.Launcher
import           Pos.Launcher.Configuration
import           Pos.Network.CLI
import           Pos.Network.Types
import           Pos.Ssc.Extra
import           Pos.Ssc.SscAlgo
import           Pos.Util.JsonLog
import           Pos.Util.UserSecret         (usVss)
import           Pos.Wallet.SscType          (WalletSscType)
import           Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.State  (WalletState)
import           Pos.WorkMode
import           System.IO
import           System.Wlog.CanLog
import           System.Wlog.LoggerName
import           System.Wlog.LoggerNameBox

newRealModeContext :: HasConfigurations => NodeDBs -> ConfigurationOptions -> Production (RealModeContext WalletSscType)
newRealModeContext dbs confOpts = do
    let nodeArgs = NodeArgs {
      sscAlgo            = GodTossingAlgo
    , behaviorConfigPath = Nothing
    }
    let networkOps = NetworkConfigOpts {
          networkConfigOptsTopology = Nothing
        , networkConfigOptsKademlia = Nothing
        , networkConfigOptsSelf     = Nothing
        , networkConfigOptsPort     = 3030
        , networkConfigOptsPolicies = Nothing
        }
    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = "node-db"
         , rebuildDB              = True
         , devSpendingGenesisI    = Nothing
         , devVssGenesisI         = Nothing
         , keyfilePath            = "secret.key"
         , backupPhrase           = Nothing
         , externalAddress        = Nothing
         , bindAddress            = Nothing
         , peers                  = mempty
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , kademliaDumpPath       = "kademlia.dump"
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , noNTP                  = True
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         }
    nodeParams <- getNodeParams cArgs nodeArgs
    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @WalletSscType @IO nodeParams gtParams $ \NodeResources{..} ->
        RealModeContext <$> pure dbs
                        <*> pure nrSscState
                        <*> pure nrTxpState
                        <*> pure nrDlgState
                        <*> jsonLogConfigFromHandle stdout
                        <*> pure (LoggerName "dbgen")
                        <*> pure nrContext
                        <*> initQueue (defaultNetworkConfig (TopologyAuxx mempty)) Nothing


walletRunner :: HasConfigurations
             => ConfigurationOptions
             -> NodeDBs
             -> WalletState
             -> UberMonad a
             -> IO a
walletRunner confOpts dbs ws act = runProduction $ do
    wwmc <- WalletWebModeContext <$> pure ws
                                 <*> liftIO (newTVarIO def)
                                 <*> newRealModeContext dbs confOpts
    runReaderT act wwmc

newWalletState :: (MonadIO m, HasConfigurations) => m WalletState
newWalletState = liftIO $ openState True "wallet-db"

defLoggerName :: LoggerName
defLoggerName = LoggerName "dbgen"

instance HasLoggerName IO where
  getLoggerName = pure defLoggerName
  modifyLoggerName _ x = x

newConfig :: ConfigurationOptions
newConfig = defaultConfigurationOptions {
    cfoSystemStart = Just 1514485290
  , cfoFilePath = "config/configuration.yaml"
  }

main :: IO ()
main = do
  let cfg = newConfig
  withConfigurations cfg $ do
    cli@CLI{..} <- getRecord "DBGen"
    dbs <- openNodeDBs False (fromMaybe "fake-db" dbPath)
    spec <- loadGenSpec config
    ws <- newWalletState
    walletRunner cfg dbs ws (generate spec)
    closeState ws

