{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


import Control.Concurrent
import Control.Monad

import           Data.List                        (isPrefixOf)
import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD

type Handler = OSC.Message -> IO ()

waitForOsc :: Handler -> IO ()
waitForOsc handler = do
  let port = 54321
  putStrLn $ "Listening for OSC messages on port " ++ show 54321
  let t = OSC.udpServer "127.0.0.1" port
  Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate not {-should be not-} $ do
    msgs <- Sound.OSC.Transport.FD.recvMessages t
    mapM_ handler msgs
    return $ any isQuitMessage msgs
  return ()
  where

isQuitMessage :: OSC.Message -> Bool
isQuitMessage m = "/comonome/quit" `isPrefixOf` OSC.messageAddress m 

statusHandler :: Handler
statusHandler m = when ("/comonome/status" `isPrefixOf` OSC.messageAddress m) $
  -- TODO print more info
  putStrLn "Fluent is alright"


main :: IO ()
main = waitForOsc (statusHandler)