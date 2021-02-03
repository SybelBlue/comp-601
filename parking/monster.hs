module Monster where

import Control.Monad ( replicateM )

getCityMap = flip replicateM getLine . read . head . words =<< getLine

squares a b = let pairs = zip <*> tail in 
    zipWith (\(a, b) (c, d) -> [a, b, c, d]) (pairs a) (pairs b)

squareRegions [a, b] = squares a b
squareRegions (r0:r1:r2:rest) = 
    squares r0 r1 ++ squares r1 r2 ++ squareRegions (r2:rest)
squareRegions _ = []

countGoodSquares n = count ((n==) . count ('X'==)) . filter (notElem '#')
    where count p = length . filter p

main = do
    city <- getCityMap
    mapM_ (print . flip countGoodSquares (squareRegions city)) [0..4]
