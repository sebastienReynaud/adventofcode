



checkRuleDouble :: [Integer] -> Bool
checkRuleDouble (a : aa : aaa) = if a ==  aa then True
                                           else checkRuleDouble $ aa : aaa                                           
checkRuleDouble _ = False                                           


--for part two--------------------------------

updatCheckDouble :: [Integer] -> Bool
updatCheckDouble    nlist       = let   reduced :: [Integer]  -> ([Integer],[Integer])
                                        reduced (a : b : ass) =  if a == b then (fst $ reduced ass, a : (snd $ reduced ass))  else (a : (fst $ reduced (b: ass)), snd $ reduced (b:ass))
                                        reduced x = (x , []) 
                                        red = reduced nlist
                                   in   elem 2 $ countInt <$> snd red <*> [nlist]
--for test
countInt :: Integer -> [Integer]  -> Int
countInt n = length.filter (==n)

reduced :: [Integer]  -> ([Integer],[Integer])
reduced (a : b : ass) =  if a == b then (fst $ reduced ass, a : (snd $ reduced ass))  else (a  : (fst $ reduced (b:ass)), snd $ reduced (b:ass))
reduced x = (x , [])                                    

----------------------------------------------

addInteger :: [Integer] -> [[Integer]]
addInteger nlist = [nlist <> [i] | i <- [last nlist..9]]

addNIntgers :: [[Integer]] -> Integer -> [[Integer]]
addNIntgers x 0 = x
addNIntgers nlist n = addNIntgers (concat $ addInteger <$> nlist) (n-1)

getN :: [[Integer]] -> Int
getN = length.filter updatCheckDouble

boundCut ::  ([Integer], [Integer]) -> [[Integer]] -> [[Integer]]
boundCut (l,u)  = filter (\x -> x >= l && x <= u) 


result :: [[Integer]] -> Int
result x = getN.boundCut ([2,4,0,9,2,0],[7,8,9,8,5,7]) $ addNIntgers x 5

main :: IO ()
main = do 
let startList = [[i] | i<- [2..7]]
putStrLn $ show $ result startList

