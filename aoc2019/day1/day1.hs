loadInput :: IO String
loadInput = readFile "../inputs/input1.txt"

fuelComp :: Int -> Int
fuelComp m = div m 3 - 2

totFuel :: [String] -> Int
totFuel (l : ls) = (fuelComp.read) l + totFuel ls 
totFuel [] = 0


main :: IO ()
main = do
l <- lines <$> loadInput
putStrLn $ show $ totFuel l