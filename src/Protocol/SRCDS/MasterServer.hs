module Network.HLDS.MasterServer
    ( Region (..)
    , Filter (..)
    , newServerList
    , queryMasterServers
    ) where

import Data.Char
import Data.Maybe
import Text.Printf

import Protocol.SRCDS.Internal.Socket
import Protocol.SRCDS.GameServer


masterServers = [ "hl1master.steampowered.com"
                , "69.28.140.247"
                , "69.28.151.162"
                , "209.197.20.34"
                , "69.28.140.245"
                , "69.28.158.131"
                ]

--
-- Region
--

data Region
    = USEastCoast
    | USWestCoast
    | SouthAmerica
    | Europe
    | Asia
    | Australia
    | MiddleEast
    | Africa
    | RestOfWorld

translateRegion :: Region -> String
translateRegion region =
    case region of
         USEastCoast  -> "\x0"
         USWestCoast  -> "\x1"
         SouthAmerica -> "\x2"
         Europe       -> "\x3"
         Asia         -> "\x4"
         Australia    -> "\x5"
         MiddleEast   -> "\x6"
         Africa       -> "\x7"
         RestOfWorld  -> "\xFF"


--
-- Filters
--

data Filter
    = Dedicated
    | Secure
    | Game String
    | Map String
    | Linux
    | NotEmpty
    | NotFull
    | HLTV
    | Empty
    | Whitelisted

translateFilter :: Filter -> String
translateFilter filter =
    case filter of
         Dedicated   -> "\\type\\d"
         Secure      -> "\\secure\\1"
         (Game mod)  -> "\\gamedir\\" ++ mod
         (Map map)   -> "\\map\\" ++ map
         Linux       -> "\\linux\\1"
         NotEmpty    -> "\\empty\\1"
         NotFull     -> "\\full\\1"
         HLTV        -> "\\proxy\\1"
         Empty       -> "\\noplayers\\1"
         Whitelisted -> "\\white\\1"


--
-- Response
--

parseResponse :: String -> [GameServer]
parseResponse cs = if "\xFF\xFF\xFF\xFF\x66\x0A" == (take 6 cs)
                     then parseResponse' $ drop 6 cs
                     else error "Malformed response from MasterServer"
  where
    parseResponse' (a:b:c:d:e:f:xs) = let host = printf "%d.%d.%d.%d" a b c d
                                          port = (ord e) * 256 + ord f
                                       in GameServer host port:parseResponse' xs
    parseResponse' []               = []
    parseResponse' _                = error "Malformed response from MasterServer"


--
-- Querying
--

-- | A pseudo GameServer which is used to start a new query operation while
-- also indicating the end of a query operation.
newServerList :: GameServer
newServerList = GameServer "0.0.0.0" 0

-- | Query a bunch of MasterServers to retrieve a list of GameServers. The
-- result may only be a subset of the matching servers in which case you
-- may use the last GameServer as starting point to a new query operation.
-- To start a new query operation you may use newServerList as start which
-- also indicates the end of a query operation.
queryMasterServers :: GameServer      -- ^ The cursor in our operation
                   -> Region
                   -> [Filter]
                   -> IO [GameServer]
queryMasterServers start region filters = do
    queryMasterServers' masterServers
  where
    query :: String
    query = printf "1%s%s:%d\x0%s\x0" (translateRegion region) (gameServerHostname start)
                (gameServerPort start) (concat $ map translateFilter filters)

    queryMasterServers' :: [String] -> IO [GameServer]
    queryMasterServers' []     = return []
    queryMasterServers' (x:xs) = do
        socket <- createSocket x 27010
        response <- interactWithSocket socket query
        case response of
             Nothing -> queryMasterServers' xs
             Just cs -> return $ parseResponse cs

-- | Chains the results of queryMasterServers to retrieve the whole
-- resulting server list.
getAllServers :: Region -> [Filter] -> IO [GameServer]
getAllServers region filters = do

