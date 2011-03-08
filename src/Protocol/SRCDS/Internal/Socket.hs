module Network.HLDS.Internal.Socket
    ( createSocket
    , interactWithSocket
    ) where


import Network.BSD
import Network.Socket
import System.Timeout


createSocket :: String -> PortNumber -> IO Socket
createSocket host port = do
    hostentry <- getHostByName host
    socket <- socket AF_INET Datagram defaultProtocol
    connect socket $ SockAddrInet port (hostAddress hostentry)
    return socket

interactWithSocket :: Socket -> String -> IO (Maybe String)
interactWithSocket socket message = do
    interactWithSocket' 3 socket message
  where
    interactWithSocket' 0 _      _       = return Nothing
    interactWithSocket' i socket message = do
        bytes <- send socket message
        response <- timeout 3000000 $ recv socket 4096
        if response == Nothing
          then interactWithSocket' (i-1) socket message
          else return response
