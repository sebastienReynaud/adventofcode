import Text.Parsec
import Data.Array
import Data.List
import Control.Monad.State


--load and parse input
loadInput :: IO String
loadInput = readFile "../inputs/input5.txt"


integer :: Parsec String () Integer
integer = (posit <|> negat)
           where posit = read <$> many digit <* char ',' 
                 negat = (\x -> - (read x ::Integer)) <$> (char '-' *> many digit <* char ',')     
    


parseInput :: String -> [Integer]
parseInput input =
    case result of
        Left e        -> error $ "l'error est ICICICICI" <> show e
        Right entries -> entries
  where
    result = parse (many integer) "" input'
    input' = (\x -> if x == '\n' then ',' else x) <$> input


-- execute code

data Code = Code 
        { code   :: Array Int Integer,
          output :: [Integer]}

    
wrightPos :: Int -> Integer -> Control.Monad.State.State Code ()
wrightPos pos val = do
    theState <- get
    let table = code theState
    put $ theState {code = table // [(pos,val)]}
    

ending ::  Control.Monad.State.State Code [Integer]
ending =  do table <- get
             return $ output table
 
getDigitsOp :: Integer -> [Integer]
getDigitsOp 0 = []
getDigitsOp n = div n 10000 : rem (div n 1000) 10 : rem (div n 100) 10 : rem (div n 10) 10 * 10 +  rem n 10 : []
 
exeOne :: Int -> Control.Monad.State.State Code [Integer]
exeOne n = do
    table <- get
    let theCode = code table
    let theOutput = output table 
    let opHead = getDigitsOp $ theCode!n
    let (opType, opModes) = (last opHead, take 3 opHead)
    let getVal :: Integer -> Integer -> Integer
        getVal mode v = if mode == 0 then theCode!(fromIntegral     v) else v 
    case opType 
      of 1 -> do wrightPos (fromInteger  $ theCode!(n+3) ) (getVal (last opModes) (theCode!(n+1)) + getVal (opModes!!1) (theCode!(n+2)))
                 exeOne (n+4)
         2 -> do wrightPos (fromInteger  $ theCode!(n+3) ) (getVal (last opModes) (theCode!(n+1)) * getVal (opModes!!1) (theCode!(n+2)))
                 exeOne (n+4)
         3 -> do wrightPos (fromInteger $ theCode!(n+1)) 5
                 exeOne (n+2)
         4 -> do put $ table { output = theOutput <> [getVal (last opModes) (theCode!(n+1))]}
                 exeOne (n+2)
         5 -> do if getVal (last opModes) (theCode!(n+1)) /= 0 then exeOne (fromInteger $ getVal (opModes!!1) (theCode!(n+2))) else exeOne (n+3)
         6 -> do if getVal (last opModes) (theCode!(n+1)) == 0 then exeOne (fromInteger $ getVal (opModes!!1) (theCode!(n+2))) else exeOne (n+3)
         7 -> do if getVal (last opModes) (theCode!(n+1))  < getVal (opModes!!1) (theCode!(n+2))  
                 then wrightPos (fromInteger  $ theCode!(n+3) ) 1 
                 else wrightPos (fromInteger  $ theCode!(n+3) ) 0
                 exeOne (n+4)
         8 -> do if getVal (last opModes) (theCode!(n+1)) == getVal (opModes!!1) (theCode!(n+2)) 
                 then wrightPos (fromInteger  $ theCode!(n+3) ) 1 
                 else wrightPos (fromInteger  $ theCode!(n+3) ) 0
                 exeOne (n+4)
         99 -> do ending
         _ -> error "unkown optype!!!"
         
         
-- testOneStep :: Int -> Control.Monad.State.State Code [Integer]
-- testOneStep = do
    -- table <- get
    -- let theCode = code table
    -- let theOutput = output table 
    -- let opHead = getDigitsOp $ theCode!n
    -- let (opType, opModes) = (last opHead, take 3 opHead)
    -- let getVal :: Integer -> Integer -> Integer
        -- getVal mode v = if mode == 0 then theCode!(fromIntegral     v) else v 
    -- case opType 
      -- of 1 -> do wrightPos (fromInteger  $ theCode!(n+3) ) (getVal (last opModes) (theCode!(n+1)) + getVal (opModes!!1) (theCode!(n+2)))
                 -- exeOne (n+4)
         -- 2 -> do wrightPos (fromInteger  $ theCode!(n+3) ) (getVal (last opModes) (theCode!(n+1)) * getVal (opModes!!1) (theCode!(n+2)))
                 -- exeOne (n+4)
         -- 3 -> do wrightPos (fromInteger $ theCode!(n+1)) 1
                 -- exeOne (n+2)
         -- 4 -> do put $ table { output = theOutput <> [getVal (last opModes) (theCode!(n+1))]}
                 -- exeOne (n+2)
         -- 99 -> do ending
         -- _ -> error "unkown optype!!!"
-- exeNounVerb :: Integer -> Integer -> Control.Monad.State.State (Array Int Integer) Integer
-- exeNounVerb noun verb = do
    -- table <- get
    -- tablesave <- get
    -- wrightPos 1 noun
    -- wrightPos 2 verb
    -- (exeOne 0)
    
-- testManyNounVerb :: (Array Int Integer) ->  [(Integer,Integer, Integer)]
-- testManyNounVerb startArr = do
         -- let totTable = [(fst (runState (exeNounVerb i j) startArr),i,j) | i <- [0..99], j <- [0..99]]
          -- in  filter (\(a,b,c) -> a==19690720) totTable 

-- testManyNounVerbtest :: (Array Int Integer) ->  [(Integer,Integer, Integer)]
-- testManyNounVerbtest startArr = do
         -- let   totTable = [(fst (runState (exeNounVerb i j) startArr),i,j)| i<-[0..13], j <- [0..13] ]
          -- in  filter (\(a,b,c) -> True) totTable         
    

main :: IO ()
main = do
m <- parseInput <$> loadInput
let marray = array (0, length m - 1 ) [(i::Int,   m!!i )| i<-[0..length m -1]]
putStrLn $ show $ fst $ runState (exeOne 0)( Code { code = marray, output = []})

