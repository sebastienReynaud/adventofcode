import Text.Parsec
import Data.List


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


main :: IO ()
main = do
--l <- solution.parseInputFINAL <$> loadInput
l <- parseInputFINAL <$> loadInput
let inter = manHatDistOrigin <$> getIntersections (getTrail (snd l) (0,0)) (getTrail (fst l) (0,0))
putStrLn $ show inter

