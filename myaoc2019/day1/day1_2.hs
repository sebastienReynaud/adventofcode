loadInput :: IO String
loadInput = readFile "../inputs/input1.txt"

fuelComp :: Int -> Int
fuelComp m = if div m 3 - 2 <= 0 then 0 else div m 3 - 2 

fuelOneModule :: Int -> Int
fuelOneModule 0 = 0
fuelOneModule n = fuelComp n + (fuelOneModule.fuelComp) n


totFuel :: [String] -> Int
totFuel (l : ls) = (fuelOneModule.read) l + totFuel ls 
totFuel [] = 0


main :: IO ()
main = do
l <- lines <$> loadInput
putStrLn $ show $ totFuel l