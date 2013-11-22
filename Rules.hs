{-# LANGUAGE BangPatterns #-}
module Main where

import Data.HProxy.Rules
import Data.HProxy.Session

session = ProxySession
    { sessionUserSID = SIDUser "user1"
    , sessionGroupsSIDs = [ SIDGroup "group1", SIDGroup "group2"]
    , sessionDate = DateYYYYMMDD 2013 11 22
    , sessionTime = TimeHHMM 00 00
    , sessionWeekDay = 4
    , sessionDestination = DestinationAddrPort (IPAddress "192.168.11.30") 1521
    }

main = do
    rules <- parseDir "rules"
    print $! matchSessionRules session rules
    return ()
