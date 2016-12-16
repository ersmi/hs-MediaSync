module MediaSyncUtils
(  
    receiveBinary,
    sendBinary,
    readConfig
) where

import Data.Text                     (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as W
import qualified Data.ByteString     as B
import Data.ConfigFile
import Data.Either.Utils            (forceEither)
import System.Info                  (os)

getFileDiv :: String
getFileDiv = case os of
                "mingw32" -> "\\"
                "linux"   -> "/"
                _         -> error ""

-- Write binary to dir/file
receiveBinary :: W.Connection -> String -> [Char] -> IO ()
receiveBinary conn file dir = do
    msg <- W.receiveData conn
    let filename = last (T.splitOn (T.pack getFileDiv) (T.pack file))
    T.putStrLn filename
    B.writeFile (dir ++ (T.unpack filename)) msg

-- Send put commands with binaries.
sendBinary :: W.Connection -> [Text] -> IO ()
sendBinary conn msglist | length msglist == 0 = putStrLn "DONE"
                        | otherwise           = do
                            bin <- B.readFile (T.unpack (head msglist))
                            let cmd = [(T.pack "put"), (head msglist)]
                            W.sendTextData conn (T.intercalate (T.pack (", ")) cmd)
                            W.sendBinaryData conn bin
                            sendBinary conn $ tail msglist

-- Get value from config file.
readConfig :: (Get_C r) => SectionSpec -> OptionSpec -> IO r
readConfig section option = do
    config <- readfile emptyCP "config.cfg"
    return $ forceEither $ get (forceEither config) section option
