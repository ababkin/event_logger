{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Monad
import Data.Aeson (eitherDecode)

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Notification
import Store (saveNotification)

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = do
  case eitherDecode (BL.pack msg) :: Either String Notification of
    Right n ->
      {- putStrLn $ show n -}
      saveNotification n
      
    Left s -> do
      putStrLn $ "Cannot parse notification: " ++ msg
      putStrLn $ "Error: " ++ s ++ "\n"

  {- putStrLn $ "From " ++ show addr ++ ": " ++ msg   -}

      


main = serveLog "5555" plainHandler



