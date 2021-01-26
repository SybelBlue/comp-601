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

data BoardTile = Ice | Rock | Diamond | Turtle

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
    { rocks   :: [Pos]
    , ice     :: [Pos]
    , diamond ::  Pos
    , turtle  ::  Pos
    , dir     ::  Dir
    }

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

data Command = Forward | TurnRight | TurnLeft | Fire

charToCommand :: Char -> Command
charToCommand 'F' = Forward
charToCommand 'R' = TurnRight
charToCommand 'L' = TurnLeft
charToCommand 'X' = Fire
charToCommand  c  = error $ "Unkown turtle command " ++ [c]

readCommands :: IO [Command]
readCommands = map charToCommand <$> getLine

type TurtleResult = Either TurtleError Board
type TurtleError = (String, Pos, Dir)

turtleError :: Board -> String -> TurtleResult
turtleError board = Left . (, turtle board, dir board)

executeCommand :: Command -> Board -> TurtleResult
executeCommand Forward board =
    if canMoveForward board 
        then return (board { turtle = nextTurtlePos board })
        else turtleError board "Collision"
executeCommand Fire (board @ Board { ice }) =
    if canFire board
        then return (board { ice = delete (nextTurtlePos board) ice })
        else turtleError board "Laser Missed"
executeCommand TurnLeft  (board @ Board { dir }) = return (board { dir = turnLeft  dir })
executeCommand TurnRight (board @ Board { dir }) = return (board { dir = turnRight dir })

runCommands :: [Command] -> Board -> TurtleResult
runCommands []     = return
runCommands (c:cs) = executeCommand c >=> runCommands cs

interpretResult :: TurtleResult -> String
interpretResult (Right Board { turtle, diamond })
    | turtle == diamond = "Diamond!"
interpretResult _       = "Bug!"

main :: IO ()
main =
  do
    board <- readBoard
    commands <- readCommands
    let result = runCommands commands board
    print $ interpretResult result
