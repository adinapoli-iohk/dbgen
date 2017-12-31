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
import           Data.Monoid
import           Data.String
import           Data.String.Conv
import qualified Data.Text                        as T
import           Lib                              (say)
import           Pos.Launcher.Configuration
import           Pos.Wallet.Web.ClientTypes.Types
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.Storage
import           Rendering
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
  let db = fromMaybe "wallet-db" walletPath
  bracket (openState False db) (\x -> closeState x >> exitSuccess) $ \db -> do
    WalletStorage{..} <- getStorage db
    let wallets  = HM.elems _wsWalletInfos
    let accounts = HM.elems _wsAccountInfos
    say $ bold "Wallets:" <> printf " %d"  (length wallets)
    listOf (map renderWallet wallets)
    blankLine
    say $ bold "Accounts:" <> printf " %d" (length accounts)
    listOf (map (caName . _aiMeta) accounts)
    say "\n"
    say $ printf "Number of used addresses: %d" (length _wsUsedAddresses)
    say $ printf "Number of change addresses: %d" (length _wsChangeAddresses)

getStorage :: ExtendedState WalletStorage -> IO WalletStorage
getStorage db = liftIO (query db GetWalletStorage)

