{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Types
import GHC.Generics
import Web.Scotty
import System.Process

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import Data.Monoid (mconcat)


data App = App { name :: String, cmd :: String, status :: Bool } deriving (Generic, Show)

instance ToJSON App where
    toEncoding = genericToEncoding defaultOptions

apps :: [App]
apps = [App "kodi" "kodi" False, App "emulationstation" "emulationstation" False]

isAllowedApp checkedName = checkedName `elem` (map name apps)

startApp :: String -> IO ()
startApp name = when (isAllowedApp name) $ do
    process <- (spawnCommand $ "nohup " ++ name ++ " &")
    return ()

stopApp :: String -> IO ()
stopApp name = when (isAllowedApp name) $ do
    process <- (callCommand $ "killall " ++ name )
    return ()

checkStatusApp :: String -> IO Bool
checkStatusApp name = do
    if (isAllowedApp name)
        then 
            do 
                (_, output, _) <- readCreateProcessWithExitCode (shell ("/usr/bin/pgrep " ++ name)) ""
                return $ not (output == "")
        else do return False

getApps :: IO [App]        
getApps = do
    statuses <- mapM (checkStatusApp . name) apps
    return $ zipWith (\x y -> App (name x) (cmd x) y) apps statuses

route :: ScottyM ()
route = do
    get "/" $ file "./static/index.html"
    get "/app/list" $ do
        appsAndStatuses <- liftIO getApps
        json appsAndStatuses
    post "/app/:name/start" $ do
        name <- param "name"
        liftIO (startApp name)
    post "/app/:name/stop" $ do
        name <- param "name"
        liftIO (startApp name)
    get "/app/:name/status" $ do
        name <- param "name"
        status <- liftIO (checkStatusApp name)
        json status

main :: IO ()
main = scotty 3000 $ route
