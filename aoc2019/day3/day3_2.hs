import Text.Parsec
import Data.List
import Control.Applicative (liftA2)


--load and parse input
loadInput :: IO String
loadInput = readFile "../inputs/input3.txt"

data Direction = R | L | U | D deriving (Eq, Show)

data Move = Move Direction Integer deriving (Show, Eq)

type Position = (Integer, Integer)



integer :: Parsec String () Integer
integer = read <$> many digit


move :: Parsec String () Move
move = (right <|> left <|> down <|> up)
  where
    right =
       Move R <$> (char 'R' *> integer <* char ',')
    left  =   
       Move L <$> (char 'L' *> integer <* char ',')
    down  =    
       Move D <$> (char 'D' *> integer <* char ',')
    up    = 
       Move U <$> (char 'U' *> integer <* char ',')


--move :: Parsec String () Move
--move = Move <$> direction <*> integer <* optional (char '\n')


parseInput :: String -> [Move]
parseInput input =
    case result of
        Left e        -> error $ show e
        Right entries -> entries
  where
    result = parse (many move) "" input
    
parseInputFINAL :: String -> ([Move],[Move])
parseInputFINAL s = (parseInput p1, parseInput p2) where p1 = takeWhile (/= '\n') s <> ","
                                                         p2 = (\x -> if x == '\n' then ',' else x)<$>  tail (dropWhile (/= '\n') s) 
                                                                                                           
                                                        
--Sovling pb
moveToTrail :: Move -> Position -> [Position]
moveToTrail (Move dir size) (xstart,ystart)=  case dir 
                                                of R -> [(i,ystart) | i <- [xstart+1..xstart+size]]
                                                   L -> [(i,ystart) | i <- [xstart-1,xstart-2..xstart-size]]
                                                   U -> [(xstart,i) | i <- [ystart+1..ystart+size]]
                                                   D -> [(xstart,i) | i <- [ystart-1, ystart-2..ystart-size]]

getTrail :: [Move] -> Position -> [Position]
getTrail (m : ms) posNow = let trail = moveToTrail m posNow
                            in trail <> getTrail ms (head.reverse $ trail)
getTrail _  _            = []


getIntersections :: [Position] -> [Position] -> [Position]
getIntersections p1 p2 = nub [pos | pos <- p1, pos `elem` p2 ]

manHatDistOrigin :: Position -> Integer
manHatDistOrigin = sum.([abs.fst,abs.snd] <*>).return

closestDist :: [Position] -> Integer
closestDist = minimum.(manHatDistOrigin <$> )

solution :: ([Move], [Move]) -> Integer
solution (p1, p2) = let t1 = getTrail p1 (0,0)
                        t2 = getTrail p2 (0,0)
                     in closestDist $ getIntersections t1 t2 
                     


addMoveToPath :: Position -> [Position] -> Maybe Position
addMoveToPath toAdd refPath = if toAdd `elem` refPath then Just toAdd else Nothing

addSecondPath :: [Position] -> [Position] -> Maybe Int -> Maybe Int
addSecondPath p1@(pfirst: ps) p2 already = case addMoveToPath pfirst p2 
                                             of Nothing -> addSecondPath ps p2 already
                                                Just x ->  if getNSteps pfirst (p1,p2) < already then addSecondPath ps p2 (getNSteps pfirst (p1,p2)) else addSecondPath ps p2 already
                                                
addSecondPath _ _  x = x                                    
                                 

addSecondPathT :: [Position] -> [Position] -> [Maybe Position] 
addSecondPathT p1@(pfirst: ps) p2  = case addMoveToPath pfirst p2 
                                       of Nothing -> addSecondPathT ps p2 
                                          Just x -> [Just pfirst] <> addSecondPathT ps p2 
addSecondPathT _ _ = []                                          
                                 

getNSteps :: Position -> ([Position],[Position]) -> Maybe Int
getNSteps p (p1,p2) = liftA2 (+) (elemIndex p p1) (elemIndex p p2)                                 
                         


main :: IO ()
main = do

l <- parseInputFINAL <$> loadInput
let p1 = (getTrail (snd l) (0,0))
let p2 = (getTrail (fst l) (0,0)) 

--let p1 = getTrail [Move R 75,Move D 30,Move R 83,Move U 83,Move L 12,Move D 49,Move R 71,Move U 7,Move L 72] (0,0)
--let p2 = getTrail [Move U 62,Move R 66,Move U 55,Move R 34,Move D 71,Move R 55,Move D 58,Move R 83] (0,0)

putStrLn $ show $ minimum $ (\(Just x) -> getNSteps x (p1,p2)) <$> addSecondPathT p1 p2  

--putStrLn $ show $ (addSecondPath p1 p2 (Just 100000), addSecondPath p2 p1    (Just 100000) )


