import Data.List


loadInput :: IO String
loadInput = readFile "../inputs/input8.txt"
-- 25 pixels wide 6 tall

cutInLayers :: String -> [String]
cutInLayers "\n" = []
cutInLayers l = take (25*6) l : cutInLayers (drop (25*6) l)  


most0 :: [String] -> String
most0 = foldr (\x y -> if count x < count y then x else y) "0000000000000000000000000000000" where count = length.filter (=='0')

resu :: String -> Int
resu x = (length.filter (=='1')) x * (length.filter (=='2')) x

main :: IO ()
main = do 
l <- cutInLayers <$> loadInput
putStrLn  $ show $ resu $most0 l
