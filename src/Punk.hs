module Punk where

import Control.Exception.Base
import Network.Socket

main' port = do
  hostaddr <- inet_addr "0.0.0.0"
  asocket <- socket AF_INET Stream defaultProtocol

  let sockaddr = SockAddrInet port hostaddr
  bindSocket asocket sockaddr
  listen asocket 1

  bracket (accept asocket) (\(conn, address) -> do
                                print conn
                                print address)
    (\(conn, _) ->
       close conn)

main = do
  main' 3000
