module Main where

import Data.Maybe
import System.IO
import Text.ParserCombinators.Parsec


data Rules = Rules FileName [(Line, Rule)]


type FileName = String
type SIDT = String

type NetT = String
type IP = String
type MaskT = Int

data RulePermission = RuleAllow
                    | RuleDeny
    deriving (Eq,Show)
allow = RuleAllow

deny = RuleDeny

data Rule = Rule RulePermission (Maybe SIDs) (Maybe Destinations) (Maybe Dates) (Maybe Times)
    deriving (Eq,Show)
data SID = SIDUser SIDT
         | SIDGroup SIDT
         -- | SIDAny
    deriving (Eq,Show)
type SIDs = [SID]

sidUser = SIDUser
sidGroup = SIDGroup
-- sidAny = SIDAny


data Destination = DestinationHost IPAddress
                 | DestinationNet IPAddress MaskT
                 | DestinationAddrPort IPAddress PortT
                -- | DestinationAny
    deriving (Eq,Show)
type Destinations = [Destination]
           
destNet = DestinationNet

destAddrPort = DestinationAddrPort

-- destAny = DestinationAny
   

data Host = Host IP
    deriving (Eq,Show)
data Mask = Mask MaskT
    deriving (Eq,Show)

data IPAddress = IPAddress IPT
    deriving (Eq,Show)
type IPT = String

data Port = Port PortT
    deriving (Eq,Show)
type PortT = Int

data Date = DateDayOfWeek Int
          | DateDaysOfWeek Int Int
          | DateRange DateYYYYMMDD DateYYYYMMDD
          | DateDay DateYYYYMMDD
    deriving (Eq,Show)
type Dates = [Date]

data Time = TimeRange TimeHHMM TimeHHMM
    deriving (Eq,Show)
type Times = [Time]  

data TimeHHMM = TimeHHMM Int Int
    deriving (Eq,Show)


data DateYYYYMMDD = DateYYYYMMDD Int Int Int
    deriving (Eq, Show)

data DayOfWeek = DayOfWeek Int
    deriving (Eq,Show)
dayOfWeek = DayOfWeek

data Hour = Hour Int
    deriving (Eq,Show)
hour = Hour

data Minute = Minute Int
    deriving (Eq,Show)
minute = Minute




--parseRule:: String -> Either ParseError RulePermission
parseRuleLine str = parse parseRule "(unknown)" str

parsePermission:: GenParser Char st RulePermission
parsePermission = do
    let allow_ = do
            spaces
            string "allow"
            return RuleAllow
        deny_ = do
            spaces
            string "deny"
            return RuleDeny
    allow_ <|> deny_

parseSIDs:: GenParser Char st SIDs
parseSIDs = do
    spaces
    string "sids"
    spaces
    char '['
    sids <- many1 $! try parseSID
    spaces
    char ']'
    return sids

parseSID:: GenParser Char st SID
parseSID = do
    let sidUser = do
            spaces
            string "user"
            spaces
            ret <- many1 alphaNum
            return $! SIDUser ret
        sidGroup = do
            spaces
            string "group"
            spaces
            many1 alphaNum >>= return . SIDGroup
    optional $! try (spaces >> char ',')
    try sidUser <|> try sidGroup

parseDestinations:: GenParser Char st Destinations
parseDestinations = do
    spaces
    string "dests"
    spaces
    char '['
    dests <- many1 $! try parseDestination
    spaces
    char ']'
    return dests

parseDestination:: GenParser Char st  Destination
parseDestination = do
    optional $! try (spaces >> char ',')
    try parseHost <|> try parseNet <|> try parseAddrPort

parseHost:: GenParser Char st Destination
parseHost = do
    spaces
    string "host"
    spaces
    ip <- parseIPAddress
    return $! DestinationHost ip

parseNet:: GenParser Char st Destination
parseNet = do
    spaces
    string "net"
    ip <- parseIPAddress
    char '/'
    mask <- count 2 digit
    return $! DestinationNet ip (read mask)

parseIPAddress :: GenParser Char st IPAddress
parseIPAddress = do
    spaces
    ret <- many $! alphaNum <|> char '.' 
    return $! IPAddress ret
    

parseAddrPort:: GenParser Char st Destination
parseAddrPort = do
    spaces
    string "addr"
    ip <- parseIPAddress
    char ':'
    port <- many1 digit
    return $! DestinationAddrPort ip (read port)


parseDates:: GenParser Char st Dates
parseDates = do
    spaces 
    string "dates"
    spaces
    char '['
    dates <- many1 $! do
         optional $! try (spaces >> char ',')
         parseDate
    spaces
    char ']'
    return dates

parseDate:: GenParser Char st Date
parseDate = do
    try parseDateRange <|> try parseWeekDay <|> try parseWeekDays <|> try parseDay
    
parseDay:: GenParser Char st Date
parseDay = do
    spaces
    string "day"
    date <- parseYYYYMMDD
    return $! DateDay date
    
parseWeekDay:: GenParser Char st Date
parseWeekDay = do
    spaces
    string "weekDay"
    spaces
    day <- count 1 digit
    return $! DateDayOfWeek (read day)


parseWeekDays:: GenParser Char st Date
parseWeekDays = do
    spaces
    string "weekDays"
    spaces
    from <- count 1 digit
    spaces
    char '-'
    spaces
    to <- count 1 digit
    return $! DateDaysOfWeek (read from) (read to)

parseYYYYMMDD:: GenParser Char st DateYYYYMMDD
parseYYYYMMDD = do
    spaces
    y <- count 4 digit
    char '.'
    m <- count 2 digit
    char '.'
    d <- count 2 digit
    return $! DateYYYYMMDD (read y) (read m) (read d)

parseDateRange:: GenParser Char st Date
parseDateRange = do
    spaces
    string "range"
    spaces
    from <- parseYYYYMMDD
    spaces
    char '-'
    spaces
    to <- parseYYYYMMDD
    return $! DateRange from to

parseTimes:: GenParser Char st Times
parseTimes = do
    spaces 
    string "times"
    spaces
    char '['
    dates <- many1 $! do
         optional $! try (spaces >> char ',')
         try parseTime
    spaces
    char ']'
    return dates

parseTime:: GenParser Char st Time
parseTime = do
    from <- parseTimeHHMM
    spaces
    char '-'
    spaces
    to <- parseTimeHHMM
    return $! TimeRange from to

parseTimeHHMM:: GenParser Char st TimeHHMM
parseTimeHHMM = do
    spaces
    h <- count 2 digit
    char ':'
    m <- count 2 digit
    return $! TimeHHMM (read h) (read m)

parseRule:: GenParser Char st Rule
parseRule = do
    permission <- parsePermission
    sids <- optionMaybe $! try parseSIDs
    dests <- optionMaybe $! try parseDestinations
    dates <- optionMaybe $! try parseDates
    times <- optionMaybe $! try parseTimes
    return $! Rule permission sids dests dates times
    
-- allow sids [user SID1] dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]
-- deny sids [user SID1] dests [ net 192.168.11.30/30 ] time [ datetime 2013.11.10 00:00]

main = do
    print $! parseRuleLine "allow sids [user SID1] dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]"
    print $! parseRuleLine "allow dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]"
    print $! parseRuleLine "allow dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]"
    print $! parseRuleLine "allow times [ 08:30 - 21:00 ]"
    print $! parseRuleLine "deny"
    -- print $! parseRuleLine "allow sids [user SID1] dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]"
    return ()
