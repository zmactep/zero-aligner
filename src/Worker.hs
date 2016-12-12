module Worker
    ( runAlignerWorker
    , WorkerConfig (..), Default (..)
    ) where

import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B (unwords, words)
import           Data.Default          (Default (..))
import           System.ZMQ4.Monadic
import           Text.Printf

import           Sequence.Alignment

data WorkerConfig = WC { host     :: String
                       , portPull :: Int
                       , portPush :: Int
                       }
  deriving (Show, Eq)

instance Default WorkerConfig where
  def = WC { host = "localhost"
           , portPull = 5556
           , portPush = 5557
           }

pullAddress :: WorkerConfig -> String
pullAddress wc = printf "tcp://%s:%d" (host wc) (portPull wc)

pushAddress :: WorkerConfig -> String
pushAddress wc = printf "tcp://%s:%d" (host wc) (portPush wc)

runAlignerWorker :: WorkerConfig -> IO ()
runAlignerWorker wc = runZMQ $ do
                        receiver <- socket Pull
                        connect receiver (pullAddress wc)

                        responder <- socket Push
                        connect responder (pushAddress wc)

                        forever $ do
                          request <- receive receiver
                          let [f, s] = B.words request
                          let (_, (f', s')) = alignment mkEditDistance f s
                          send responder [] $ B.unwords [f', s']
                          return ()
