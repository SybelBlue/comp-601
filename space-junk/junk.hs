import Data.Tuple (fst, snd)

minDay :: [Int] -> Int
minDay [] = 0
minDay (x:xs) = fst . foldr least (1, x) $ zip [2..] xs
    where least a b = if snd b < snd a then b else a

main :: IO ()
main = print =<< minDay . map read . words <$> (getLine *> getLine)
-- the first input is the number of numbers in the next line, uneccessary
