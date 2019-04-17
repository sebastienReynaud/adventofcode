loadInput :: IO String
loadInput = readFile "inputs/day-2.txt"

countreRep :: String -> [Int]
countreRep ctot@(c : cs) = let nf = length $ filter ( == c) ctot in
                        (nf : countreRep (filter ( /= c) cs))
countreRep _ = [0]


filterCount2 :: [Int] -> Int
filterCount2 (a : ass) = if a == 2 then 1 else filterCount2 ass
filterCount2 _ =0

filterCount3 :: [Int] -> Int
filterCount3 (a : ass) = if a == 3 then 1 else filterCount3 ass
filterCount3 _ = 0

processInput :: String -> Int
processInput l = sum n2 * sum n3                 
                    where ll = lines l 
                          numberlist = countreRep <$> ll
                          n2 = filterCount2 <$> numberlist
                          n3 = filterCount3 <$> numberlist

main :: IO ()
main = do
l <- loadInput
print $ processInput l
