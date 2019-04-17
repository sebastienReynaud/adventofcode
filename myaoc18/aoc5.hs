import Data.Char

loadInput :: IO String
loadInput = readFile "inputs/day-5.txt"





reduce :: Char -> Char ->  [Char]
reduce a b  
            | (toUpper a == b || toUpper b == a) && a /= b = []
            | otherwise = [a, b]

compose :: [Char] -> [Char] -> [Char]            
compose a@(fa : ass) b@(fb : bs) 
            | reduce lstA fb == [lstA, fb]  =  a <> b 
            | otherwise =  compose aWOlst bs
                where lstA = (head.reverse) a
                      aWOlst = take (length a - 1) a
compose a b = a <> b                 

fullReduce :: [Char] -> [Char]
fullReduce chain = foldr compose [] segments
                    where segments = ( : []) <$> chain

                      
main :: IO ()
main = do

l <- loadInput

print  $ fullReduce l 