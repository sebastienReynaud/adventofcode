import Data.Function
import Data.Array
import Data.List
import           Text.Parsec         (Parsec, char, digit, many, optional,
                                      parse, string)

                                      
loadInput :: IO String
loadInput = readFile "inputs/day-6.txt"


newtype Coord =
    Coord (Integer, Integer)
    deriving (Show, Eq)

distance :: Coord -> Coord -> Integer
distance (Coord (x1,y1)) ( Coord (x2,y2)) = abs (x1-x2) + abs (y1-y2)     

-- Parsing stuff
integer :: Parsec String () Integer
integer = read <$> many digit

coord :: Parsec String () Coord
coord =
    curry Coord <$> (integer <* string ", ") <*> integer <* optional (char '\n')

parseInput :: String -> [Coord]
parseInput input =
    case result of
        Left e       -> error $ show e
        Right coords -> coords
  where
    result = parse (many coord) "" input

--actual pb stuff

theMap :: [Coord] -> Array (Integer, Integer) Coord
theMap coords = array ((xMin,yMin),(xMax, yMax)) [((i,j), Coord (i,j)) | i <- [xMin..xMax], j<-[yMin..yMax]]
                   where [xMin, xMax] = [minimum, maximum] <*>  [((\(Coord (x,y)) -> x) <$> coords)]
                         [yMin, yMax] = [minimum, maximum] <*>  [((\(Coord (x,y)) -> y) <$> coords)]

                          
bins :: [Coord] -> [(Coord, Integer)] 
bins = map (\coord ->(coord, 0))

addOne :: Coord -> [(Coord, Integer)] -> [(Coord, Integer)]
addOne c cbins = if   c == (fst.head) cbins 
                 then (fst prem, snd prem + 1) : tail cbins 
                 else  if tail cbins == [] then error "coord addOne inexistante" else prem : addOne c (tail cbins) 
                    where prem = head cbins

fillBin :: Coord -> [(Coord, Integer)] -> [(Coord, Integer)]
fillBin c cbins = let  dlist = (\(coord, n) -> (coord, distance c coord)) <$> cbins
                       dlsorted = sortBy (\x y -> (compare `on ` snd) x y) dlist
                       sndclose = (snd.head.tail) dlsorted
                   in if sndclose == (snd.head) dlsorted
                       then cbins
                       else addOne ((fst.head) dlsorted) cbins                          

parseMap :: [(Coord, Integer)] -> Array (Integer, Integer) Coord -> [(Coord, Integer)]
parseMap cbins aMap = foldr (\coord cb -> fillBin coord cb) cbins aMap
    
isInfinte :: Coord -> Array (Integer, Integer) Coord -> [Coord]-> Bool
isInfinte c@(Coord (x,y)) ar coords = if head dlsorted <= closestBoun
                                    then False
                                    else True
                                        where [xMin, xMax]  =  [fst $ fst $ bounds ar, fst $ snd $ bounds ar]
                                              [yMin, yMax]  =  [snd $ fst $ bounds ar, snd $ snd $ bounds ar]
                                              dlist         = (\coord ->  distance c coord) <$> coords
                                              dlsorted        = sort dlist
                                              closestX      = min (x-xMin) (xMax -x)
                                              closestY      = min (y- yMin) (yMax -y)
                                              closestBoun   = min closestX closestY
                               

    
largestArea :: [(Coord, Integer)] -> Array (Integer, Integer) Coord -> Coord 
largestArea coordlist a = case isInfinte (fst prem) a (fst <$> coordlist) of 
                         False -> fst $ prem
                         True  -> largestArea (tail ordlist) a 
                        where ordlist = sortBy (\x y -> (compare `on ` snd) y x) coordlist
                              prem = head ordlist
                        
                        
largestAreaLazyAss :: [(Coord, Integer)] -> Array (Integer, Integer) Coord -> Integer
largestAreaLazyAss coordlist a = case isInfinte (fst prem) a (fst <$> coordlist) of 
                         False -> snd $ prem
                         True  -> largestAreaLazyAss (tail ordlist) a 
                        where ordlist = sortBy (\x y -> (compare `on ` snd) y x) coordlist
                              prem = head ordlist                 
                             
   
main :: IO ()
main = do 
l <- parseInput <$> loadInput 
let mamap = theMap l  
let thebins = reverse $ parseMap (bins l) mamap
print $ "The largest area is arround coord:"
print $  largestArea thebins mamap 
print $ "and its area is :"
print $ largestAreaLazyAss thebins mamap
print $ "The filled bins are :"
print $ thebins
