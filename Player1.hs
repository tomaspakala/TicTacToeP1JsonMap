module Player1 where

import JsonMapMove
import WinnerSearch
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BytCh
import qualified Data.ByteString.Lazy.Char8 as BytChLazy
import qualified System.IO as SysIO
import qualified Data.CaseInsensitive as CI

main :: IO ()
main = do
    postMoves ""
    SysIO.putStrLn "Good Game"

url1 = "http://tictactoe.homedir.eu/game/tp_mj9/player/1"

toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

postMoves msg = do
    let opts = defaults & header (toHeaderName "Content-Type") .~ [BytCh.pack "application/json+map"]
    let msg' = addMoves msg
    r <- postWith opts url1 $ BytCh.pack (msg')
    if isGameOver msg'
        then return()
        else getMoves

getMoves = do
    let opts2 = defaults & header (toHeaderName "Accept") .~ [BytCh.pack "application/json+map"]
    r <- getWith opts2 url1
    let msg = BytChLazy.unpack(r ^. responseBody)
    if isGameOver msg
        then return()
        else postMoves msg