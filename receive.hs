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

waitForOsc :: Handler -> IO ()
waitForOsc handler = do
  let port = 15144
  putStrLn $ "Listening for OSC messages on port " ++ show port
  let t = OSC.udpServer "127.0.0.1" port
  Sound.OSC.Transport.FD.withTransport t $ \t -> void $ OSC.untilPredicate not {-should be not-} $ do
    msgs <- Sound.OSC.Transport.FD.recvMessages t
    mapM_ handler msgs
    return $ any isQuitMessage msgs
  return ()
  where

isQuitMessage :: OSC.Message -> Bool
isQuitMessage m = "/comonome/quit" `isPrefixOf` OSC.messageAddress m 

monomeStringHandler :: Handler
monomeStringHandler m = when ("/monome/grid/led/set" `isPrefixOf` OSC.messageAddress m) $ go m
  where
    go (OSC.Message _ [OSC.ASCII_String x, OSC.ASCII_String y, OSC.ASCII_String s])
      = putStrLn $ "got strings: x,y,z = " ++ show x -- >> return ()
    go _ = return ()

monomeIntHandler :: Handler
monomeIntHandler m = when ("/monome/grid/led/set" `isPrefixOf` OSC.messageAddress m) $ go m
  where
    go (OSC.Message _ [OSC.Int32 x, OSC.Int32 y, OSC.Int32 s])
      = putStrLn $ "got ints: x,y,z = " ++ show s -- >> return () 
    go _ = return ()     

statusHandler :: Handler
statusHandler m = when ("/comonome/status" `isPrefixOf` OSC.messageAddress m) $ go m
  where go m = putStrLn $ "Comonome is getting it"

composeHandlers :: [Handler] -> Handler
composeHandlers = foldr composeHandlers2 (\x -> return ())

composeHandlers2 :: Handler -> Handler -> Handler
composeHandlers2 f g m = do
  f m
  g m  

main :: IO ()
main = waitForOsc $ composeHandlers [monomeStringHandler, monomeIntHandler, statusHandler]