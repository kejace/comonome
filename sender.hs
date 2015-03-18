{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


import Control.Concurrent
import Control.Monad

import           Data.ByteString.Char8            (ByteString (..))
import qualified Data.ByteString.Char8            as BS

import           Data.List                        (isPrefixOf)
import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD

type Handler = OSC.Message -> IO ()

kOSCDest = ("127.0.0.1", 54321)

sendSomeOsc :: IO ()
sendSomeOsc = do

  oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port
  let port = 54321
  putStrLn $ "Sending some OSC on " ++ show port
  let t = OSC.udpServer "127.0.0.1" port
  let msg = OSC.Message "/comonome/status" (fmap (OSC.ASCII_String . BS.pack) ["hello"])
  OSC.sendMessage "/"
  return ()
  -- Sound.OSC.Transport.FD.sendMessage msg

main :: IO ()
main = sendSomeOsc