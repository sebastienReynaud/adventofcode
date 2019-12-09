import Text.Parsec
import Data.Array
import Data.List
import Control.Monad.State


--load and parse input
loadInput :: IO String
loadInput = readFile "../inputs/input2.txt"


integer :: Parsec String () Integer
integer = read <$> many digit <* char ',' 


parseInput :: String -> [Integer]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many integer) "" input'
    input' = (\x -> if x == '\n' then ',' else x) <$> input


-- execute code
    
wrightPos :: Int -> Integer -> Control.Monad.State.State (Array Int Integer) ()
wrightPos pos val = do
    table <- get
    put $ table // [(pos,val)]
    
firstStep :: Control.Monad.State.State (Array Int Integer) ()
firstStep =  do 
    table <- get
    put $ table // [(1,12)] // [(2,2)]

ending ::  Control.Monad.State.State (Array Int Integer) Integer
ending =  do table <- get
             return $ table!0
    
exeOne :: Int -> Control.Monad.State.State (Array Int Integer) Integer
exeOne n = do
    table <- get
    let [opType, el1, el2, wriPos] = (table!) <$> [n..n+3]
    case opType 
      of 1 -> do wrightPos (fromInteger wriPos) (table!(fromInteger el1) + table!(fromInteger el2))
                 exeOne (n+4)
         2 -> do wrightPos (fromInteger wriPos) (table!(fromInteger el1) * table!(fromInteger el2))
                 exeOne (n+4)
         99 -> do ending
         _ -> error "unkown optype!!!"

exeNounVerb :: Integer -> Integer -> Control.Monad.State.State (Array Int Integer) Integer
exeNounVerb noun verb = do
    table <- get
    tablesave <- get
    wrightPos 1 noun
    wrightPos 2 verb
    (exeOne 0)
    
testManyNounVerb :: (Array Int Integer) ->  [(Integer,Integer, Integer)]
testManyNounVerb startArr = do
         let totTable = [(fst (runState (exeNounVerb i j) startArr),i,j) | i <- [0..99], j <- [0..99]]
          in  filter (\(a,b,c) -> a==19690720) totTable 

testManyNounVerbtest :: (Array Int Integer) ->  [(Integer,Integer, Integer)]
testManyNounVerbtest startArr = do
         let   totTable = [(fst (runState (exeNounVerb i j) startArr),i,j)| i<-[0..13], j <- [0..13] ]
          in  filter (\(a,b,c) -> True) totTable         
    

main :: IO ()
main = do
m <- parseInput <$> loadInput
let marray = array (0, length m - 1 ) [(i::Int,   m!!i )| i<-[0..length m -1]]
putStrLn $ show  $ testManyNounVerb marray
