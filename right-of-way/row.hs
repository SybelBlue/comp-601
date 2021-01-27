data Dir = North | South | East | West deriving (Eq, Show, Read)

right :: Dir -> Dir
right North = East
right East = South
right South = West
right West = North

isRightOf :: Dir -> Dir -> Bool
isRightOf = (==) . right

isOppositeOf :: Dir -> Dir -> Bool
isOppositeOf = isRightOf . right

isLeftOf :: Dir -> Dir -> Bool
isLeftOf = isOppositeOf . right

-- yield if:
-- - You want to pass straight through the intersection; 
--   another vehicle is approaching from your right.
-- - You want to turn left at the intersection; 
--   another vehicle is approaching from the opposite 
--   direction or from your right.
yieldRightOfWay :: Dir -> Dir -> Dir -> Bool
yieldRightOfWay pos target approach
    | pos `isOppositeOf` target && approach `isRightOf` pos = True
    | target `isLeftOf` pos && not (approach `isLeftOf` target) = True
yieldRightOfWay _ _ _ = False

getInput :: IO (Dir, Dir, Dir)
getInput = intoTuple <$> map read <$> words <$> getLine
    where
        intoTuple [a, b, c] = (a, b, c)
        intoTuple _ = error "Improper number of directions given"

main :: IO ()
main = do
    (pos, target, approach) <- getInput
    let result = yieldRightOfWay pos target approach
    putStrLn (if result then "Yes" else "No")
