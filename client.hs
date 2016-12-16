{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent           (forkIO)
import Control.Monad                (forever, unless)
import Control.Monad.Trans          (liftIO)
import Data.Text                    (Text)
import Network.Socket               (withSocketsDo)
import System.Directory             (getDirectoryContents)
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as W

import MediaSyncUtils

app :: W.ClientApp ()
app conn = do
    putStrLn "Connected!"
    printHelp
    hostname <- readConfig "CLIENT" "hostname"
    W.sendTextData conn (T.pack hostname)
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- W.receiveData conn
        dir <- readConfig "CLIENT" "dir"
        case msg of
            _   | op == (T.pack "rb") -> do
                    -- Send file to server
                    liftIO $ putStrLn "Client: Pushing binaries to server:"
                    sendBinary conn msglist
                | op == (T.pack "put") -> do
                    -- Save file from server.
                    receiveBinary conn ((map T.unpack msglist) !! 0) dir 
                | otherwise -> do
                    -- Otherwise, print informational message from server
                    liftIO $ putStrLn $ T.unpack msg
                where 
                    msglist = tail $ T.splitOn ", " msg
                    op = head $ T.splitOn ", " msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            dir <- readConfig "CLIENT" "dir"
            case line of
                _   | line == "ls" -> do
                        -- Send request to server for folder diffs
                        str <- ( getDirectoryContents dir)
                        let s = ["ls", dir] ++ (reverse (drop 2 (reverse str)))
                        W.sendTextData conn (T.intercalate (", ") (map T.pack s)) >> loop
                    | line == "push" -> do
                        -- Send request to server to prepare for binaries
                        str <- (getDirectoryContents dir)
                        let s = ["push", dir] ++ (reverse (drop 2 (reverse str)))
                        W.sendTextData conn (T.intercalate (", ") (map T.pack s)) >> loop
                    | line == "pull" -> do
                        -- Send request for binaries from server.
                        str <- (getDirectoryContents dir)
                        let s = ["pull"] ++ (reverse (drop 2 (reverse str)))
                        W.sendTextData conn (T.intercalate (", ") (map T.pack s)) >> loop
                    | line == "exit" -> do
                        -- Disconnect from server.
                        disconnect conn hostname 
                    | otherwise -> unless (T.null line) $ W.sendTextData conn line >> loop
    loop

disconnect :: W.Connection -> [Char] -> IO ()
disconnect conn hostname = W.sendClose conn (T.pack $ hostname ++ " disconnected.")

printHelp = do
    putStrLn $ "ls \t - Display files unique to each dir\n" ++
               "push \t - Send files unique to client to the server\n" ++
               "pull \t - Get files unique to server\n" ++
               "exit \t - Exit\n"

main :: IO ()
main = do
    host <- readConfig "CLIENT" "hostname"
    port <- readConfig "CLIENT" "port"
    withSocketsDo $ W.runClient host port "/" app
