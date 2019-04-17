loadInput :: IO String
loadInput = readFile "inputs/day-3.txt"

data Rectangle = Rectangle
    { rectId     :: Integer
    , rectLeft   :: Integer
    , rectTop    :: Integer
    , rectWidth  :: Integer
    , rectHeight :: Integer
    } deriving (Show)

stopAt :: Char -> String -> String
stopAt c ( f  : cs ) = if f == c then
                            ""
                            else
                            f : stopAt c cs
stopAt _ cs = cs                           

suppressSpaces :: String -> String
suppressSpaces = filter ( /= ' ') 
    
claimToRect :: String -> Rectangle
claimToRect ('#' : xs) = let a = stopAt '@' xs
                             b = stopAt ',' $ drop (1 + length a) xs
                             c = stopAt ':' $ drop (2 + length a + length b) xs 
                             d = stopAt 'x' $ drop (3 + length a + length b + length c) xs
                             e = drop (4 + length a + length b + length c + length d) xs
                             in Rectangle (read a :: Integer) (read b :: Integer) (read c :: Integer) (read d :: Integer) (read e :: Integer)
claimToRect _ = Rectangle 0 0 0 0 0 

rectOverlap :: Rectangle -> Rectangle -> [(Integer, Integer)]
rectOverlap r1 r2 = case (firstFromTop + firstFromTopWidth > secondFromTop, firstFromLeft + firstFromLeftWidth > secondFromLeft) of
                        (True, True) -> [(x,y) | x <- [(secondFromLeft+1)..(min (secondFromLeft+secondFromLeftWidth) (firstFromLeft + firstFromLeftWidth))] , y <-  [(secondFromTop+1)..(min (secondFromTop+secondFromTopWidth) (firstFromTop + firstFromTopWidth))]]
                        _ -> []
                        where firstFromLeft = minimum $ rectLeft <$> [r1, r2] 
                              firstFromLeftWidth = if firstFromLeft == rectLeft r1 then rectWidth r1 else rectWidth r2
                              secondFromLeft =  maximum $ rectLeft <$> [r1, r2] 
                              secondFromLeftWidth =  if firstFromLeft == rectLeft r1 then rectWidth r2 else rectWidth r1
                              firstFromTop = minimum $ rectTop <$> [r1, r2] 
                              firstFromTopWidth = if firstFromTop == rectTop r1 then rectTop r1 else rectTop r2
                              secondFromTop = maximum $ rectTop <$> [r1, r2]
                              secondFromTopWidth =if firstFromLeft == rectLeft r1 then rectWidth r2 else rectWidth r1

                              
                              
getListOverlap :: [Rectangle] -> [(Integer, Integer)]
getListOverlap (r1 : rs) = (concat list1 ) ++ getListOverlap rs
                                where list1 = (rectOverlap r1) <$> rs                             
getListOverlap _ = []                              

countOverlap :: [(Integer, Integer)] -> Integer
countOverlap (coord1 : coords) = if coord1 `elem` coords then 1 + rest else rest 
                                    where rest = countOverlap (filter ( /= coord1) coords)
countOverlap _ = 0



main :: IO ()
main = do
l <- loadInput
putStrLn $ show $ (countOverlap.getListOverlap) $ claimToRect <$> (lines.suppressSpaces) l