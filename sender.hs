{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


import Control.Concurrent
import Control.Monad

import System.Posix.Unistd
import System.Posix

import           Data.ByteString.Char8            (ByteString (..))
import qualified Data.ByteString.Char8            as BS

import           Data.List                        (isPrefixOf)
import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD

type Handler = OSC.Message -> IO ()

kOSCDest = ("127.0.0.1", 54321)

data Action
  = SendOSC
      String [String] -- address port pattern args (i.e. "/fluent/play" ["n1","1"])

sendMoreOSC :: Action -> IO ()
sendMoreOSC = go
  where
    go (SendOSC patt args) = forever $ do
      putStrLn "Sending some OSC"
      blockSignals $ addSignal sigVTALRM emptySignalSet
      sleep 2
      let msg = OSC.Message patt (fmap (OSC.ASCII_String . BS.pack) args)
      oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port
      -- putStrLn $ "Sending " ++ show msg
      Sound.OSC.Transport.FD.sendMessage oscOut $ msg

sendSomeOsc :: IO ()
sendSomeOsc = do

  oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port
  let port = 54321
  putStrLn $ "Sending some OSC on " ++ show port
  let t = OSC.udpServer "127.0.0.1" port
  let msg = OSC.Message "/comonome/status" (fmap (OSC.ASCII_String . BS.pack) ["hello"])
  --Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate not {-should be not-} $ do
  --  msgs <- Sound.OSC.Transport.FD.sendMessage msg t
  return ()

main :: IO ()
main = sendMoreOSC $ SendOSC "/comonome/status" ["hello"]