{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.Monad

import System.Posix.Unistd
import System.Posix

import qualified Data.ByteString.Char8            as BS

import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD


kOSCDest = ("127.0.0.1", 15144)

data Action
  = SendOSC
      String [OSC.Datum] -- address port pattern args (i.e. "/fluent/play" ["n1","1"])

sendMoreOSC :: Action -> IO ()
sendMoreOSC = go
  where
    go (SendOSC patt args) = forever $ do
      putStrLn "Sending some OSC"
      blockSignals $ addSignal sigVTALRM emptySignalSet
      sleep 2
      --let msg = OSC.Message patt (fmap (OSC.ASCII_String . BS.pack) args)
      let msg = OSC.Message patt args
      oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port
      -- putStrLn $ "Sending " ++ show msg
      Sound.OSC.Transport.FD.sendMessage oscOut $ msg

main :: IO ()
main = sendMoreOSC $ SendOSC "/monome/grid/led/set" [OSC.Int32 1, OSC.Int32 1, OSC.Int32 1]