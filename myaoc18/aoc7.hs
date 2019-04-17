import           Data.List
import           Text.Parsec         (Parsec, char, many, optional, parse,
                                      string, upper)

loadFile :: IO String
loadFile = readFile "inputs/day-7.txt"

newtype Dependency =
    Dependency (Char, Char)
    deriving (Eq, Show)

-- Parsing
dependency :: Parsec String () Dependency
dependency =
    mkDependency <$>
    (string "Step " *> upper <* string " must be finished before step ") <*>
    (upper <* string " can begin.") <*
    optional (char '\n')
  where
    mkDependency a b = Dependency (a, b)

parseInput :: String -> [Dependency]
parseInput input =
    case result of
        Left e             -> error $ show e
        Right dependencies -> dependencies
  where
    result = parse (many dependency) "" (prepare input)
    prepare = unlines . nub . sort . lines
    
    
--the problem
  

getAvailables :: [Dependency] -> [Char] -> [Char]
getAvailables deps donelist = sort $ nub $ [fstdep c | c <- deps , notElem (fstdep c) (snddep <$> deps) , fstdep c `notElem` donelist] <> [snddep c | c <- deps , fstdep c `elem` donelist, snddep c `notElem` donelist]
                                where snddep = (\(Dependency x) -> snd x)
                                      fstdep = (\(Dependency x) -> fst x)
                                      
                
                              
doStep :: [Char] -> [Dependency] -> ([Char], [Dependency])   
doStep donelist deps = (donelist <> [doneStep], upDatedDeps)
                            where upDatedDeps = filter (\(Dependency x) -> snd x /= doneStep) deps
                                  avs = getAvailables deps donelist 
                                  doneStep = head avs 
                           

gothru :: [Char] -> [Dependency] -> [Char]
gothru donelist deps = if deps == []
                       then donelist 
                       else uncurry gothru stepOut
                        where avs = getAvailables deps donelist
                              stepOut = doStep donelist deps
                              

    
main :: IO ()
main = do

l <- parseInput <$> loadFile

print $  gothru [] l    