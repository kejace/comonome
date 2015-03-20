{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad

import Control.Concurrent

import qualified Data.ByteString.Char8            as BS

import qualified Sound.OSC                        as OSC
import qualified Sound.OSC.Transport.FD

import System.Random
import Control.Monad (replicateM)

import GHC.Int

kOSCDest = ("127.0.0.1", 15144)

data Action
  = SendOSC
      String [OSC.Datum] -- address port pattern args (i.e. "/fluent/play" ["n1","1"])

sendMoreOSC :: Action -> IO ()
sendMoreOSC (SendOSC patt args) = forever $ do
      putStrLn "Sending some OSC"

      threadDelay 5000

      --rand <- randomRIO (0::Int, 7::Int)
      g <- newStdGen
      let (rx) = fst $ randomR (0::Int, 7::Int) g
      g <- newStdGen
      let (ry) = fst $ randomR (0::Int, 7::Int) g
      g <- newStdGen
      let (rs) = fst $ randomR (0::Int, 1::Int) g
      --let msg = OSC.Message patt [OSC.Int32 $ fromIntegral (rx :: Int), OSC.Int32 2, OSC.Int32 1]
      let msg = OSC.Message patt (fmap (\x -> OSC.Int32 $ fromIntegral (x :: Int)) [rx, ry, rs])
      oscOut <- let (addr, port) = kOSCDest in OSC.openUDP addr port

      Sound.OSC.Transport.FD.sendMessage oscOut $ msg


main :: IO ()
main = sendMoreOSC $ SendOSC "/monome/grid/led/set" [OSC.Int32 1, OSC.Int32 1, OSC.Int32 1]