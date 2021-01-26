import Data.Tuple (fst, snd)
import Data.List (elemIndex)

readIntLine :: IO Int
readIntLine = read <$> getLine

readInts :: IO [Int]
readInts = map read <$> words <$> getLine

minIndex :: [Int] -> Int
minIndex [] = -1
minIndex (x:xs) = fst $ foldr folder (0, x) $ zip [1..] xs
    where folder a b = if snd b < snd a then b else a

main :: IO ()
main = do
    days <- readIntLine
    junkLevels <- readInts
    let goodDay = 1 + minIndex (take days junkLevels)
    print goodDay
