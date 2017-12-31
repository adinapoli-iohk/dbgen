
module Rendering where

import qualified Data.HashMap.Strict              as HM
import           Data.Monoid
import qualified Data.Text                        as T
import           Lib                              (say)
import           Pos.Wallet.Web.ClientTypes.Types
import           Pos.Wallet.Web.State.Storage
import           System.Console.ANSI
import           Text.Printf

renderWallet :: WalletInfo -> T.Text
renderWallet WalletInfo{..} = T.pack $
  printf "%s, %s, %s, PendingTxs: %s" (bold $ T.unpack $ cwName _wiMeta)
                                      (renderReady _wiIsReady)
                                      (renderSync _wiSyncTip)
                                      (renderPendingTxs _wsPendingTxs)

renderSync :: WalletTip -> String
renderSync wt = case wt of
  NotSynced    -> red "NotSynced"
  SyncedWith h -> green "Synced" <> printf "[%s]" (show h)

renderPendingTxs :: HM.HashMap a b -> String
renderPendingTxs m = case HM.size m of
  0 -> green "0"
  x -> yellow (show x)

colored :: Color -> String -> String
colored col txt = setSGRCode [Reset, SetColor Foreground Dull col] <> txt <> setSGRCode [Reset]

red :: String -> String
red = colored Red

bold :: String -> String
bold txt = setSGRCode [Reset, SetConsoleIntensity BoldIntensity] <> txt <> setSGRCode [Reset]

yellow :: String -> String
yellow = colored Red

green :: String -> String
green = colored Green

renderReady :: Bool -> String
renderReady True  = green "ready"
renderReady False = yellow "restoring"

listOf :: [T.Text] -> IO ()
listOf = putStrLn . T.unpack . mappend "\n- " . T.intercalate "\n- "

blankLine :: IO ()
blankLine = say "\n"
