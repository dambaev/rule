{-# LANGUAGE BangPatterns #-}
module Main where

import Data.HProxy.Rules
import Data.HProxy.Session
import Control.Monad

session = ProxySession
    { sessionUserSID = SIDUser "user1"
    , sessionGroupsSIDs = [ SIDGroup "group1", SIDGroup "group2"]
    , sessionDate = DateYYYYMMDD 2013 11 22
    , sessionTime = TimeHHMM 09 00
    , sessionWeekDay = 4
    , sessionDestination = DestinationAddrPort (IPAddress "192.168.11.30") 1521
    }

main = do
    rules <- parseDir "rules"
    forM rules $! \rule-> do
        putStrLn $! show rule
        putStrLn ""
    print ""
    print $! matchSessionRules session rules
    return ()
