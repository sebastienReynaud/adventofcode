import Text.Parsec hiding (State)
import Data.List
import Control.Monad.State 


data Depend  = Depend { father :: String,
                        son :: String} deriving (Show, Eq, Ord) -- father then kid
  

type Chain = [String]                       

--load and parse input
loadInput :: IO String
loadInput = readFile "../inputs/input6.txt"

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
processDeps deps cs = let heads = head <$> cs
                          (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                          sameFat = if hasHead == [] then error ("me manque pere" <> show cs) else groupBy (\x y -> father x == father y) (sort hasHead)
                          addODep :: [[Depend]] -> [Chain] -> [Chain]
                          addODep [] chains  = chains
                          addODep (d : ds) clist = addODep ds $ clist >>= (\y -> if head y == (father.head) d then (\x -> (son x :)) <$> d <*> [y] else [y] )  
                          csUpdated = addODep sameFat cs                                         
                       in processDeps hasNotHead csUpdated  
  
-- debuging fun
-- doOneStep :: [Depend] -> [Chain] -> [Chain] 
-- doOneStep deps cs = let   heads = head <$> cs
                          -- (hasHead, hasNotHead) = partition (\x -> father x `elem` heads ) deps
                          -- sameFat = if hasHead == [] then error ("me manque pere" <> show cs) else groupBy (\x y -> father x == father y) hasHead
                          -- addODep :: [[Depend]] -> [Chain] -> [Chain]
                          -- addODep [] chains  = chains
                          -- addODep (d : ds) clist = addODep ds $ clist >>= (\y -> if head y == (father.head) d then (\x -> (son x :)) <$> d <*> [y] else [y] )  
                          -- csUpdated = addODep sameFat cs                                         
                   -- in     csUpdated 
                                      
                                   
                                      
countN :: State (Integer,([Chain],[String])) Integer
countN = do
 (tot,(toAdd,added)) <- get
 if toAdd == []
 then do return tot
 else let getWeight :: Chain -> Integer
          getWeight [] = 0
          getWeight (c :cs) = if c `elem` added  
                            then 0
                            else  if cs /= [] then fromIntegral (length $ cs) + getWeight cs else 0
                               
       in do let totnew = tot + getWeight (head toAdd)                       
             put $ (totnew, (tail toAdd, nub $ added <> head toAdd))
             countN  
           


getCommon :: Chain -> Chain -> String
getCommon c1 c2 = head $ dropWhile (\x -> not $ elem x c2 ) c1       
     
pathLength :: String -> String -> State [Chain] Int
pathLength s1 s2 = do 
 chains <- get
 let c1 = dropWhile (/=s1)$ head $ filter (elem s1) chains         
 let c2 = dropWhile (/=s2) $ head $ filter (elem s2) chains         
 let comon = getCommon c1 c2
 return $ (\(Just x) (Just y)->x+y) (elemIndex comon c1)  (elemIndex comon c2)  
 

 
main :: IO ()
main = do 
deplist <- parseInput <$> loadInput
let processed = snd $ processDeps deplist []
         

putStrLn $ show $  fst $ runState (pathLength "YOU" "SAN") processed
