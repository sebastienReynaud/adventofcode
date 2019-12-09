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
    firstStep
    table <- get
    let [opType, el1, el2, wriPos] = (table!) <$> [n..n+3]
    case opType 
      of 1 -> do wrightPos (fromInteger wriPos) (table!(fromInteger el1) + table!(fromInteger el2))
                 exeOne (n+4)
         2 -> do wrightPos (fromInteger wriPos) (table!(fromInteger el1) * table!(fromInteger el2))
                 exeOne (n+4)
         99 -> do ending
         _ -> error "unkown optype!!!"

    
    

main :: IO ()
main = do
m <- parseInput <$> loadInput
let marray = array (0, length m - 1 ) [(i::Int,   m!!i )| i<-[0..length m -1]]
let mtest = array (0,8) [(0,1),(1,1),(2,1),(3,4),(4,99),(5,5),(6,6),(7,0),(8,99)] :: Array Int Integer
--putStrLn $ show $ runState (exeOne 0) mtest
putStrLn $ show  $ runState (exeOne 0) marray
