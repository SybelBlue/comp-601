import Data.Set (Set, empty, insert)
import Data.Either (either)

readNLines x = (:) <$> getLine <*> (if x <= 0 then return [] else readNLines (x - 1))

nthUnique n = foldl unique (Left empty) . zip [1..]
    where
        unique (Left set) (day, item) = let new = insert item set in
            if n == length new then Right day else Left new
        unique res _ = res

main = do
    [parts, purchased] <- map read . words <$> getLine
    lines <- readNLines purchased
    either (const $ putStrLn "Paradox avoided") print $ nthUnique parts lines
