{-# LANGUAGE BangPatterns #-}
module Main where

import Data.HProxy.Rules
import Data.HProxy.Session
import Network.AD.SID
import Control.Monad

session = ProxySession
    { sessionUserSID = SIDUser "S-1-5-21-1085031214-2146703713-725345543-12922"
    , sessionGroupsSIDs = [ SIDGroup "group1", SIDGroup "group2"]
    , sessionDate = DateYYYYMMDD 2013 11 22
    , sessionTime = TimeHHMM 09 00
    , sessionWeekDay = 4
    , sessionDestination = DestinationAddrPort (IPAddress "192.168.11.30") 2010
    }

main = do
    rules <- parseRuleDir "rules"
    forM rules $! \rule-> do
        putStrLn $! show rule
        putStrLn ""
    print ""
    print $! matchSessionRules session rules
    return ()
