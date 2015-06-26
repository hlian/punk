module Main where

import qualified Data.ByteString.Char8 as C

import BasePrelude
import Data.ByteString
import Network.Socket hiding (recv)
import Network.Socket.ByteString

data Rq = Rq { p :: ByteString, h :: [(ByteString, ByteString)] } deriving (Show)

trim = C.filter (\c -> c /= ' ' && c /= '\r')

elemAt :: Int -> [a] -> Maybe a
elemAt _ []     = Nothing
elemAt 0 (x:_)  = Just x
elemAt i (_:xs) = elemAt (i-1) xs

header s =
  let parts = C.split ':' s in
  (elemAt 0 parts, elemAt 1 parts)

request s =
  let (head:rest) = C.split '\n' s
      parts = C.split ' ' head in
  Rq (parts !! 1) [(i, trim j) | (Just i, Just j) <- header <$> rest]

l skt =
  bracket (accept skt) (close . fst) $ \(c, _) -> do
    incoming <- recv c 1024
    print (request incoming)

main' port = do
  skt <- socket AF_INET Stream defaultProtocol
  setSocketOption skt ReuseAddr 1
  bindSocket skt (SockAddrInet port iNADDR_ANY)
  listen skt 2
  forever (l skt)

main = do
  main' 3001
