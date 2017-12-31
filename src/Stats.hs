{-# LANGUAGE TypeFamilies #-}
module Stats where

import           CLI
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Acid                        as A
import qualified Data.Acid.Core                   as A
import           Data.Functor.Identity
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe
import           Data.String
import           Data.String.Conv
import qualified Data.Text                        as T
import           Lib                              (say)
import           Pos.Launcher.Configuration
import           Pos.Wallet.Web.ClientTypes.Types
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.Storage
import           Serokell.AcidState.ExtendedState
import           System.Exit
import           Text.Printf

{-
data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    -- @_wsBalances@ depends on @_wsUtxo@,
    -- it's forbidden to update @_wsBalances@ without @_wsUtxo@
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    }
-}

showStatsAndExit :: HasConfigurations => CLI -> IO ()
showStatsAndExit CLI{..} = do
  say "Printing stats of the current wallet:"
  let db = fromMaybe "wallet-db" walletPath
  bracket (openState False db) (\x -> closeState x >> exitSuccess) $ \db -> do
    WalletStorage{..} <- getStorage db
    let wallets  = HM.elems _wsWalletInfos
    let accounts = HM.elems _wsAccountInfos
    say $ printf "Wallets: %d"  (length wallets)
    listOf (map renderWallet wallets)
    say "\n"
    say $ printf "Number of accounts: %d" (length accounts)
    listOf (map (caName . _aiMeta) accounts)

listOf :: [T.Text] -> IO ()
listOf = putStrLn . T.unpack . mappend "\n- " . T.intercalate "\n- "

getStorage :: ExtendedState WalletStorage -> IO WalletStorage
getStorage db = liftIO (query db GetWalletStorage)

renderWallet :: WalletInfo -> T.Text
renderWallet WalletInfo{..} = T.pack $
  printf "%s, %s, PendingTxs: %s" (cwName _wiMeta)
                                  (renderSync _wiSyncTip)
                                  (renderPendingTxs _wsPendingTxs)

renderSync :: WalletTip -> String
renderSync wt = case wt of
  NotSynced    -> "NotSynced"
  SyncedWith h -> printf "Synced[%s]" (show h)

renderPendingTxs :: HM.HashMap a b -> String
renderPendingTxs = show . HM.size

{--
data WalletInfo = WalletInfo
    { _wiMeta         :: !CWalletMeta
    , _wiPassphraseLU :: !PassPhraseLU
    , _wiCreationTime :: !POSIXTime
    , _wiSyncTip      :: !WalletTip
    , _wsPendingTxs   :: !(HashMap TxId PendingTx)
    -- Wallets that are being synced are marked as not ready, and
    -- are excluded from api endpoints. This info should not be leaked
    -- into a client facing data structure (for example `CWalletMeta`)
    , _wiIsReady      :: !Bool
    }
--}
