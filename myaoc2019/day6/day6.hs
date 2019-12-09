import Text.Parsec
import Data.List


data Depend  = Depend { father :: String,
                        son :: String} deriving (Show, Eq) -- father then kid
  

type Chain = [String]                       

--load and parse input
loadInput :: IO String
loadInput = readFile "../inputs/input6Test.txt"

depend :: Parsec String () Depend
depend = Depend <$> many (noneOf ")") <* char ')'  <*> many (noneOf "\n")  <* optional (char '\n')



parseInput :: String -> [Depend]
parseInput input =
    case result of
        Left e        -> error $ "l'error est au parsing" <> show e
        Right entries -> entries
  where
    result = parse (many depend) "" input

-- assemble list of dependences


processDeps :: [Depend] -> [Chain] -> ([Depend],[Chain])
processDeps deps [] = let  heads = ["COM"]
                           (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                           csUpdated = ((\x y -> if head y == father x then son x : y else y) <$> hasHead ) <*> [["COM"]]
                       in  processDeps hasNotHead csUpdated
processDeps [] cs = ([], cs)    
processDeps deps@(d:ds) cs = let  heads = head <$> cs
                                  (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                                  csUpdated = nub $ cs <> ( ((\x y -> if head y == father x then son x : y else []) <$> hasHead ) <*> cs)
                              in  processDeps hasNotHead csUpdated
                          

step1 deps [] = let heads = ["COM"]
                    (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                    csUpdated = ((\x y -> if head y == father x then son x : y else y) <$> hasHead ) <*> [["COM"]]
                 in csUpdated

step2 deps@(d:ds) cs =       let  heads = head <$> cs
                                  (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                                  csUpdated = ((\x y -> if head y == father x then son x : y else []) <$> hasHead ) <*> cs
                              in  csUpdated           
                          
count :: [Chain] -> Int
count n = 4                         
                    
main :: IO ()
main = do 
deplist <- parseInput <$> loadInput
let processed = snd $ processDeps deplist []
let proc1step = step2 deplist [["B","COM"]]
let proc1step2 = step2 deplist [["C","B","COM"],["G","B","COM"]]                

putStrLn $ show $ processed