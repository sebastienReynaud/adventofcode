{-# LANGUAGE RecordWildCards #-}

import           Data.Function
import           Data.List
import           Text.Parsec

loadInput :: IO String
loadInput = readFile "inputs/day-4.txt"

data Event
    = ShiftStart Integer
    | FallAsleep
    | WakeUp
    deriving (Show, Eq)

data Timestamp = Timestamp
    { tsYear   :: Integer
    , tsMonth  :: Integer
    , tsDay    :: Integer
    , tsHour   :: Integer
    , tsMinute :: Integer
    } deriving (Show, Eq, Ord)

data Entry = Entry
    { entryTimestamp :: Timestamp
    , entryEvent     :: Event
    } deriving (Show, Eq)

instance Ord Entry where
    a <= b = entryTimestamp a <= entryTimestamp b

-- Parsing stuff
integer :: Parsec String () Integer
integer = read <$> many digit

timestamp :: Parsec String () Timestamp
timestamp =
    Timestamp <$> (char '[' *> integer <* char '-') <*> (integer <* char '-') <*>
    (integer <* char ' ') <*>
    (integer <* char ':') <*>
    (integer <* char ']')

event :: Parsec String () Event
event = char ' ' *> (shiftStart <|> fallsAsleep <|> wakesUp)
  where
    shiftStart =
        ShiftStart <$> (string "Guard #" *> integer <* string " begins shift")
    fallsAsleep = FallAsleep <$ string "falls asleep"
    wakesUp = WakeUp <$ string "wakes up"

entry :: Parsec String () Entry
entry = Entry <$> timestamp <*> event <* optional (char '\n')

parseInput :: String -> [Entry]
parseInput input =
    case result of
        Left e        -> error $ show e
        Right entries -> entries
  where
    result = parse (many entry) "" input

-- Actual problem

data Guard = Guard 
            { guardId :: Integer
            ,  guardStart :: [Timestamp]
            ,  guardSleeps :: [Timestamp]
            ,  guardWakes ::[Timestamp] }
            deriving (Eq, Show)
           
            
processEntry :: Entry -> Integer -> [Guard] -> (Integer, [Guard])
processEntry (Entry tstamp (ShiftStart n)) nold gds =  let thisguard = filter (== n) (guardId <$> gds) in
                                                            case thisguard of
                                                                []  -> (n, (Guard n [tstamp] [] [] : gds))
                                                                [g] -> (n, (\guy@(Guard id start sleep wakes) -> if id == n then Guard id (start <> [tstamp]) sleep wakes else guy) <$> gds)
                                                                _   -> error "several guards bins with same ID"
processEntry (Entry tstamp WakeUp) nold gds = let thisguard = filter (== nold) (guardId <$> gds) in
                                                    case thisguard of
                                                     [g] -> (nold, (\guy@(Guard id start sleep wakes) -> if id == nold then Guard id start sleep (wakes <> [tstamp]) else guy) <$> gds)
                                                     _   -> error "guard not named before wakes"


processEntry (Entry tstamp FallAsleep) nold gds = let thisguard = filter (== nold) (guardId <$> gds) in
                                                    case thisguard of
                                                     [g] -> (nold, (\guy@(Guard id start sleep wakes) -> if id == nold then Guard id start (sleep <> [tstamp]) wakes else guy) <$> gds)
                                                     _   -> error "guard not named before wakes"

                                                     
fillGuardBin :: [Entry] -> Integer -> [Guard] -> [Guard] 
fillGuardBin (e : es) n gds = fillGuardBin es (fst processed) (snd processed) --start at guard = [] if no id is up
                                where processed = processEntry e n gds
fillGuardBin [] n gds = gds   

countAsleep :: Guard -> Integer
countAsleep (Guard id starts (f : falls) (w : wakes)) 
    | ([tsYear, tsMonth, tsDay, tsHour] <*> [f]) == ([tsYear, tsMonth, tsDay, tsHour] <*> [w]) = (head.tail) mins - head mins + (countAsleep (Guard id starts falls wakes)) 
    | otherwise = error "one falls without wakup" 
       where mins = tsMinute <$> [f, w] 
countAsleep  _ = 0


bestMinute :: Guard -> Integer
bestMinute (Guard id starts fff@(f : falls) www@(w : wakes))
            | ([tsYear, tsMonth, tsDay, tsHour] <*> [f]) == ([tsYear, tsMonth, tsDay, tsHour] <*> [w]) = let go :: [Timestamp] -> [Timestamp] -> [Integer]
                                                                                                             go (s : sleeps) (ww : wakes) =  [head mins .. (head.tail) mins] <> go sleeps wakes where  mins = tsMinute <$> [s, ww]
                                                                                                             go [] _ = []
                                                                                                             go _ [] = []                                                                                                                                                                                                               
                                                                                                             numberlist = go fff www
                                                                                                             listCoupl =  (\x -> (head x, length x)) <$> (group.sort) numberlist
                                                                                                             bestMin = fst $ foldr (\x y -> if  snd x >= snd y then x else y) (0,0) listCoupl
                                                                                                         in  bestMin  
                                                                                                       
                                                                                                              
            | otherwise = error "one falls without wakup in the bestGuard"
bestMinute (Guard id (s : starts) [] []) = 0             
bestMinute _ = error "Missed pattern in Bestminute"         
            

soluTION :: [Entry] -> Integer
soluTION es = let finalGlist = (fillGuardBin.sort) es (-1) [] 
                  bestGuard = foldr (\g1 g2 -> if  (countAsleep g1) >= (countAsleep g2) then g1 else g2) (Guard (-1) [] [] []) finalGlist 
                  
               in guardId bestGuard *  bestMinute bestGuard                               
                                                     
main :: IO ()
main = do
-- he has 43 as best mins
-- i have 44
input <- parseInput <$> loadInput
print $ soluTION input
