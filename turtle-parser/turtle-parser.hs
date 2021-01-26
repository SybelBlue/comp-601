-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Maybe (mapMaybe)
import Data.List (delete)

import Control.Monad ((>=>))

readNLines :: Int -> IO [String]
readNLines x
    | x <= 0 = return []
readNLines x = (:) <$> getLine <*> readNLines (x - 1)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

type Pos = (Int, Int)

pos :: Int -> Int -> Pos
pos = (,)

addPos :: Pos -> Pos -> Pos
addPos (a, i) (b, j) = (a + b, i + j)

data BoardTile = Ice | Rock | Diamond | Turtle deriving Show

charToTile :: Char -> Maybe BoardTile
charToTile 'C' = Just Rock
charToTile 'I' = Just Ice
charToTile 'D' = Just Diamond
charToTile 'T' = Just Turtle
charToTile  _  = Nothing

readBoardTiles :: IO [[Maybe BoardTile]]
readBoardTiles = map (map charToTile) <$> readNLines 8

tilePositions :: [[Maybe BoardTile]] -> [(Pos, BoardTile)]
tilePositions board = concat $ map repack $ reverse . enumerate . reverse $ map enumerate board
    where repack (r, row) = mapMaybe (\(c, mTile) -> (pos r c, ) <$> mTile) row

data Dir = U | D | R | L deriving Show

dirDelta :: Dir -> Pos
dirDelta U = ( 1,  0)
dirDelta D = (-1,  0)
dirDelta R = ( 0,  1)
dirDelta L = ( 0, -1)

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

data Board = Board
    { rocks :: [Pos]
    , ice :: [Pos]
    , diamond :: Pos
    , turtle :: Pos
    , dir :: Dir
    }
    deriving Show

blank :: Board
blank = Board { rocks = [], ice = [], diamond = (0, 0), turtle = (0, 0), dir = R }

addTile :: Board -> (Pos, BoardTile) -> Board
addTile (board @ Board { rocks }) (p, Rock) = board { rocks = p : rocks }
addTile (board @ Board { ice }) (p, Ice) = board { ice = p : ice }
addTile board (p, Diamond) = board { diamond = p }
addTile board (p, Turtle) = board { turtle = p }

fromTiles :: [(Pos, BoardTile)] -> Board
fromTiles = foldl addTile blank

readBoard :: IO Board
readBoard = fromTiles . tilePositions <$> readBoardTiles

nextTurtlePos :: Board -> Pos
nextTurtlePos Board { turtle, dir } = addPos turtle $ dirDelta dir

canMoveForward :: Board -> Bool
canMoveForward (board @ Board { rocks, ice, turtle, dir }) = not $ nextTurtlePos board `elem` (rocks ++ ice)

canFire :: Board -> Bool
canFire (board @ Board { ice }) = nextTurtlePos board `elem` ice

data Command = Forward | TurnRight | TurnLeft | Fire deriving Show

charToCommand :: Char -> Command
charToCommand 'F' = Forward
charToCommand 'R' = TurnRight
charToCommand 'L' = TurnLeft
charToCommand 'X' = Fire
charToCommand  c  = error $ "Unkown turtle command " ++ [c]

readCommands :: IO [Command]
readCommands = map charToCommand <$> getLine

executeCommand :: Command -> Board -> Maybe Board
executeCommand Forward board =
    if canMoveForward board 
        then Just (board { turtle = nextTurtlePos board })
        else Nothing
executeCommand Fire (board @ Board { ice }) =
    if canFire board
        then Just (board { ice = delete (nextTurtlePos board) ice })
        else Nothing
executeCommand TurnLeft (board @ Board { dir }) = Just (board { dir = turnLeft dir })
executeCommand TurnRight (board @ Board { dir }) = Just (board { dir = turnRight dir })

runCommands :: [Command] -> Board -> Maybe Board
runCommands []     = return
runCommands (c:cs) = executeCommand c >=> runCommands cs

interpretResult :: Maybe Board -> String
interpretResult (Just (Board { turtle, diamond }))
    | turtle == diamond = "Diamond!"
interpretResult _       = "Bug!"

main :: IO ()
main =
  do
    board <- readBoard
    commands <- readCommands
    let result = runCommands commands board
    print $ interpretResult result
