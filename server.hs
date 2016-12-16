{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception            (finally)
import Control.Monad                
import Control.Monad.Trans          (liftIO)
import Data.Either.Utils            (forceEither)
import Data.List                    (union, (\\), delete)
import System.Directory             (getDirectoryContents)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as W

import MediaSyncUtils

-- typedefs from WS example.
type Client = (T.Text, W.Connection)
type Clients = [Client]

-- helper function from WS example.
addClient :: Client -> Clients -> Clients
addClient c cs = cs ++ [c]

-- Send message function from WS example.
broadcast :: T.Text -> Clients -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> W.sendTextData conn message

-- Handshake, modified from the WS example.
app :: MVar Clients -> W.ServerApp
app state pending = do
    conn <- W.acceptRequest pending
    W.forkPingThread conn 30
    clients <- readMVar state
    msg <- W.receiveData conn
    case msg of 
        _ | otherwise -> flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
                let s' = addClient client s
                W.sendTextData conn $
                    ("User Connected:" :: T.Text) `mappend` T.intercalate "," (map fst s')
                return s'
            talk conn state client
            where
                client = (msg, conn)
                disconnect = do
                    s <- readMVar state
                    broadcast "User disconnected." s

-- Apply functions
talk :: W.Connection -> MVar Clients -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- W.receiveData conn
    case msg of
        _   | op == "ls" -> do 
                -- Return response with list of files unique to each folder.
                let filelist = tail msglist
                diffs <- lsfolders filelist
                readMVar state >>= broadcast (user `mappend` ": " `mappend` "Server: Received ls for " `mappend` (msglist !! 0) `mappend` "\n" `mappend` diffs)
            | op == "push" -> do
                -- Prepare to receive files from client.
                diffs <- lsfolders msglist
                -- Parse ls response to get only files to push.
                let df = T.splitOn (T.pack "client:\n") diffs
                let dflist = tail $ T.splitOn (T.pack "\n") (last df)
                case dflist of 
                    _   | (length dflist) == 0 -> do
                            readMVar state >>= broadcast ("Server: No unique files on client. Push failed.")
                        | otherwise -> do
                            readMVar state >>= broadcast ("rb, " `mappend` T.intercalate ", " (map (T.append ( clientdir)) dflist))
            | op == "pull" -> do
                -- Send binaries to client.
                diffs <- getpullfiles msglist
                case diffs of
                    _   | (length diffs) == 0 -> do
                            readMVar state >>= broadcast ("Server: No unique files on server. Pull failed.")
                        | otherwise -> do
                            liftIO $ putStrLn "Server: Pushing binaries to client..."
                            T.putStrLn (head msglist)
                            sendBinary conn diffs
            | op == "put" -> do
                -- Save binaries from client.
                dir <- readConfig "SERVER" "dir"
                receiveBinary conn ((map T.unpack msglist) !! 0) dir
                readMVar state >>= broadcast ("Server: Server received file.")
            | otherwise -> readMVar state >>= broadcast ("Server: " `mappend` "Bad command from " `mappend` user `mappend` ": " `mappend` msg)
            where 
                msglist = tail $ T.splitOn ", " msg
                op = head (T.splitOn ", " msg)
                clientdir = head (msglist)

-- Get list of files to send to client
getpullfiles msglist = do
     dir <- readConfig "SERVER" "dir"
     files <- ( getDirectoryContents dir )
     let dirlist = map T.pack (reverse (drop 2 (reverse files)))
     let dirmsglist = (union) dirlist msglist
     let ret = (dirmsglist \\ msglist) 
     let tdir = T.pack (dir)
     return $ map (tdir `mappend`) ret
     
-- PP the files unique to each folder.
lsfolders :: [T.Text] -> IO T.Text
lsfolders msglist = do
     dir <- readConfig "SERVER" "dir"
     files <- ( getDirectoryContents dir )
     let dirlist = map T.pack (reverse (drop 2 (reverse files)))
     let dirmsglist = (union) dirlist msglist
     let msgunique = dirmsglist \\ dirlist
     let dirunique = dirmsglist \\ msglist
     return $ "Files unique to server:\n" `T.append` T.intercalate "\n" dirunique `T.append` "\n" `T.append` "Files unique to client:\n" `T.append` T.intercalate "\n" msgunique

main :: IO ()
main = do
    state <- newMVar []
    host <- readConfig "SERVER" "hostname"
    port <- readConfig "SERVER" "port"
    W.runServer host port (app state)
