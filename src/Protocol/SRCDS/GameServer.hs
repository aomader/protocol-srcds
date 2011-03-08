module Network.HLDS.GameServer
    ( GameServer (..)
    , GameServerType (..)
    , GameServerOS (..)
    , GameServerInfo (..)
    , pingGameServer
    , aboutGameServer
    ) where

import Data.Char
import Network.Socket
import System.Time

import Protocol.SRCDS.Internal.Socket

data GameServer = GameServer
    { gameServerHostname     :: String
    , gameServerPort         :: Int
    }
  deriving (Show)

data GameServerType
    = DedicatedServer
    | ListenServer
    | SourceTV
  deriving (Show)

data GameServerOS
    = LinuxOS
    | WindowsOS
  deriving (Show)

data GameServerInfo = GameServerInfo
    { serverVersion     :: Int
    , serverName        :: String
    , serverMap         :: String
    , serverMod         :: String
    , serverModDesc     :: String
    , serverAppId       :: Int
    , serverPlayers     :: Int
    , serverMaxPlayers  :: Int
    , serverBots        :: Int
    , serverType        :: GameServerType
    , serverOS          :: GameServerOS
    , serverPassword    :: Bool
    , serverSecure      :: Bool
    , serverGameVersion :: String
    }
  deriving (Show)


queryGameServer :: GameServer -> String -> IO (Maybe String)
queryGameServer server message = do
    socket <- createSocket (gameServerHostname server) (fromIntegral $ gameServerPort server)
    response <- interactWithSocket socket message
    sClose socket
    return response


pingGameServer :: GameServer -> IO (Maybe Float)
pingGameServer server = do
    TOD ss sp <- getClockTime
    response <- queryGameServer server "\xFF\xFF\xFF\xFFi"
    TOD es ep <- getClockTime
    case response of
        Just "\xFF\xFF\xFF\xFFj\NUL" -> return $ Just (0.1)
        Nothing                      -> return Nothing
        otherwise                    -> error "Malformed response from GameServer"

aboutGameServer :: GameServer -> IO (Maybe GameServerInfo)
aboutGameServer server = do
    response <- queryGameServer server "\xFF\xFF\xFF\xFFTSource Engine Query\NUL"
    case response of
        Just ('\xFF':'\xFF':'\xFF':'\xFF':'I':cs) -> return (Just $ parseSourceAbout cs)
        Just ('\xFF':'\xFF':'\xFF':'\xFF':'m':cs) -> return (Just $ parseGoldAbout cs)
        Nothing                                   -> return Nothing
        otherwise                                 -> error "Malformed response from GameServer"
  where
    parseSourceAbout xs = let (a:as) = xs
                              (b, _:bs) = span (/= '\NUL') as
                              (c, _:cs) = span (/= '\NUL') bs
                              (d, _:ds) = span (/= '\NUL') cs
                              (e, _:es) = span (/= '\NUL') ds
                              (f:g:h:i:j:k:l:m:n:ns) = es
                              (o, _) = span (/= '\NUL') ns
                           in GameServerInfo { serverVersion = ord a
                                             , serverName = b
                                             , serverMap = c
                                             , serverMod = d
                                             , serverModDesc = e
                                             , serverAppId = ord g * 256 + ord f
                                             , serverPlayers = ord h
                                             , serverMaxPlayers = ord i
                                             , serverBots = ord j
                                             , serverType = case k of
                                                                 'l' -> ListenServer
                                                                 'd' -> DedicatedServer
                                                                 'p' -> SourceTV
                                             , serverOS = case l of
                                                               'l' -> LinuxOS
                                                               'w' -> WindowsOS
                                             , serverPassword = if m == '\x01' then True else False
                                             , serverSecure = if n == '\x01' then True else False
                                             , serverGameVersion = o
                                             }
    parseGoldAbout xs = let (a, _:as) = span (/= '\NUL') xs
                            (b, _:bs) = span (/= '\NUL') as
                            (c, _:cs) = span (/= '\NUL') bs
                            (d, _:ds) = span (/= '\NUL') cs
                            (e, _:es) = span (/= '\NUL') ds
                            (f:g:h:i:j:k:l:m:n:ns) = es
                         in GameServerInfo { serverVersion = -1
                                           , serverName = b
                                           , serverMap = c
                                           , serverMod = d
                                           , serverModDesc = e
                                           , serverAppId = -1
                                           , serverPlayers = ord f
                                           , serverMaxPlayers = ord g
                                           , serverBots = -1
                                           , serverType = case i of
                                                               'l' -> ListenServer
                                                               'd' -> DedicatedServer
                                                               'p' -> SourceTV
                                           , serverOS = case j of
                                                             'l' -> LinuxOS
                                                             'w' -> WindowsOS
                                           , serverPassword = if k == '\x01' then True else False
                                           , serverSecure = False
                                           , serverGameVersion = "asd"
                                           }

