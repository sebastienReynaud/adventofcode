loadInput :: IO String
loadInput = readFile "inputs/day-1.txt"



lineToFunction :: String -> Integer -> Integer
lineToFunction ('+' : n) = (+) (read n :: Integer)
lineToFunction ('-' : n) = (-) (read n :: Integer)


totalList :: [String] -> Integer
totalList l = foldr (\f x-> f x) 0 (lineToFunction <$> l) 


main :: IO ()
main = do

l <- loadInput
putStrLn $ (show . totalList . lines) l

    