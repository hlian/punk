module Main where

import BasePrelude
import Control.Exception.Base
import Network.Socket hiding (recv)
import Network.Socket.ByteString

run conn = do
  ok <- recv conn 1024
  print ok

sisyphus asocket =
  bracket (accept asocket) (\(conn, _) -> close conn) $ \(conn, _) -> do
    mm <- recv conn 1024
    print mm

main' port = do
  asocket <- socket AF_INET Stream defaultProtocol
  setSocketOption asocket ReuseAddr 1
  bindSocket asocket (SockAddrInet port iNADDR_ANY)
  listen asocket 2
  forever (sisyphus asocket)

main = do
  main' 3001
