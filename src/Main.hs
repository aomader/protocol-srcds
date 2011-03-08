module Main where

import Network.Socket
import Text.Printf

import Protocol.SRCDS.MasterServer
import Protocol.SRCDS.GameServer

main :: IO ()
main = withSocketsDo $ do
    getServers newServerList
  where
    checkServer (server:xs) = do
        ping <- pingGameServer server
        xasd <- aboutGameServer server
        case xasd of
            Nothing  -> putStrLn $ printf "%s:%d\t DEAD" (gameServerHostname server) (gameServerPort server)
            Just asd -> putStrLn $ printf "%s:%d\t%-32s\t%-20s\t%d/%d" (gameServerHostname server) (gameServerPort server) (serverName asd) (serverMap asd) (serverPlayers asd) (serverMaxPlayers asd)
        if null xs
          then getServers server
          else checkServer xs
    checkServer _ = putStrLn "Done"
    getServers seed = do
        servers <- queryMasterServers seed Europe [Dedicated, Secure, Linux, Game "cstrike", NotEmpty, NotFull]
        checkServer servers
