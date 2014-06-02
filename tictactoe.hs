-- We want to program the game of tic tac toe, also known as XO

import Control.Applicative
import System.IO

lineLength = 3
initialPlayer = PlayerOne

maybeFallback (Just newVal) _ = newVal
maybeFallback Nothing fallback = fallback

flatten [] = []
flatten (ys:xs) = ys ++ (flatten xs)

class Renderable a where
    render :: a -> IO ()

data Field = Empty | X | O deriving (Eq)

fieldToString Empty = "."
fieldToString X = "X"
fieldToString O = "O"

instance Renderable Field where
    render x = putStr $ fieldToString x

instance Show Field where
    show f = fieldToString f

fieldToPlayer X = PlayerOne
fieldToPlayer O = PlayerTwo
fieldToPlayer Empty = PlayerNone

data Line = Line [Field] deriving (Eq)

makeLine xs = Line xs
getLineList (Line l) = l
lineFieldAt line index = (getLineList line) !! index
lineSetFieldAt l n newElem = makeLine $ iterateLine (getLineList l) n
    where iterateLine l 0 = newElem:(tail l)
          iterateLine l n = (head l):(iterateLine (tail l) (n - 1))

makeNewLine = makeLine $ take lineLength $ repeat Empty

instance Renderable Line where
    render l = putStr $ (show l) ++ "\n"

instance Show Line where
    show l = flatten (map show (getLineList l))

data Board = Board [Line] deriving (Eq)

makeBoardOfList lines = Board lines
getBoardLines (Board lines) = lines
changeBoardField board (x,y) newValue = makeBoardOfList $ iterateBoard (getBoardLines board) y
    where iterateBoard (line:lines) 0 = (lineSetFieldAt line x newValue):lines
          iterateBoard (line:lines) n = line:(iterateBoard lines (n - 1))

makeNewBoard = makeBoardOfList (take lineLength $ repeat makeNewLine)

instance Renderable Board where
    render b = do
      putStr $ unlines $ map show (getBoardLines b)
      putStr "\n"

instance Show Board where
    show b = unlines $ map show $ map getLineList (getBoardLines b)

boardFieldAt board x y
    | validCoord x && validCoord y = Just (lineFieldAt (line y) x)
    | otherwise = Nothing
    where line n = (getBoardLines board) !! n


boardToString n [] = ""
boardToString n (b:bs)
    | n == (lineLength - 1) = (fieldToString b) ++ "\n" ++ boardToString 0 bs
    | otherwise = (fieldToString b) ++ boardToString (n + 1) bs

renderBoard g = do
  render (getGameBoard g)
  return g

data Player = PlayerOne | PlayerTwo | PlayerNone deriving (Eq, Show)

playerToField PlayerOne = X
playerToField PlayerTwo = O
playerToField PlayerNone = Empty

otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne
otherPlayer PlayerNone = PlayerNone

playerToString PlayerOne = "Player X"
playerToString PlayerTwo = "Player O"
playerToString PlayerNone = "No Player"

data Message = Message String

getMessage (Message m) = m
makeMessage m = Message m

instance Show Message where
    show m = getMessage m

instance Renderable Message where
    render m = putStr (getMessage m ++ "\n")

renderMessages (Game board player []) = return (Game board player [])
renderMessages (Game board player (m:ms)) = do
  render m
  renderMessages (Game board player ms)

data Game = Game Board Player [Message] deriving (Show)

getGamePlayer (Game _ player _) = player
getGameBoard (Game board _ _) = board
getGameMessages (Game _ _ messages) = messages

won (Game board player _)
    | playerWon board PlayerOne = Left PlayerOne
    | playerWon board PlayerTwo = Left PlayerTwo
    | otherwise = Right False
    where playerWon board player = rows || columns || diagonals
              where rows = (checkRow 0) || (checkRow 1) || (checkRow 2)
                        where checkRow n = (isOfPlayer 0 n) && (isOfPlayer 1 n) && (isOfPlayer 2 n)
                    columns = or [(checkCol 0),(checkCol 1),(checkCol 2)]
                        where checkCol n = and [(isOfPlayer n 0) , (isOfPlayer n 1) , (isOfPlayer n 2)]
                    diagonals = or [diagonalTopLeft, diagonalTopRight]
                        where diagonalTopLeft = (isOfPlayer 0 0) && (isOfPlayer 1 1) && (isOfPlayer 2 2)
                              diagonalTopRight = (isOfPlayer 0 2) && (isOfPlayer 1 1) && (isOfPlayer 2 0)
                    isOfPlayer x y = if (player == (maybe PlayerNone fieldToPlayer (boardFieldAt board x y))) then True else False
                              
                               

coordinatesToIndex x y = x + ( lineLength *y)

validCoord x
    | x < 0 = False
    | x > 2 = False
    | otherwise = True

-- This method is intended to translate numbers from the numpad to
-- coordinates in a 3x3 game of tictactoe.
-- 7 8 9
-- 4 5 6
-- 1 2 3
numpadToCoord :: Int -> Maybe (Int,Int)
numpadToCoord 7 = Just (0,0)
numpadToCoord 4 = Just (0,1)
numpadToCoord 1 = Just (0,2)
numpadToCoord 8 = Just (1,0)
numpadToCoord 5 = Just (1,1)
numpadToCoord 2 = Just (1,2)
numpadToCoord 9 = Just (2,0)
numpadToCoord 6 = Just (2,1)
numpadToCoord 3 = Just (2,2)
numpadToCoord x = Nothing

coordToNumpad (x,y) = (3*(2-y)+x)+1

strToNum = (convStrToNum . reverse)
convStrToNum [] = pure 0
convStrToNum (x:xs) = pure (+) <*> charToInt x <*> (pure (10 * ) <*> convStrToNum xs)

charToInt '0' = pure 0
charToInt '1' = pure 1
charToInt '2' = pure 2
charToInt '3' = pure 3
charToInt '4' = pure 4
charToInt '5' = pure 5
charToInt '6' = pure 6
charToInt '7' = pure 7
charToInt '8' = pure 8
charToInt '9' = pure 9
charToInt x = Nothing

-- Input of Data

getCoordinates inputprompt = do
  input <- prompt inputprompt
  result $ coordinates input
  where coordinates x = strToNum x >>= numpadToCoord
        result Nothing = getCoordinates inputprompt
        result (Just x) = return x
  
processTurn (Game board player messages) coordinates = Game newboard newplayer newmessages
    where newboard = changeBoardField board coordinates (playerToField player) 
          newplayer = if turnfail then player else otherPlayer player
          newmessages = if turnsuccess then messages else (makeMessage "Invalid Turn"):messages
          turnfail = (newboard == board)
          turnsuccess = not turnfail

checkInput game = do
  putStr $ playerGreeting (getGamePlayer game)
  coordinates <- (getCoordinates "Where? ")
  return (processTurn game coordinates)


playerGreeting p = "It is " ++ playerToString p ++ "s turn!\n"

gameLoop oldGame@(Game board player messages) = do
 renderBoard oldGame >>= renderMessages >>= checkInput >>= nextTurn
  where nextTurn game 
            | won game  == Left PlayerOne = renderVictory game PlayerOne
            | won game  == Left PlayerTwo = renderVictory game PlayerTwo
            | otherwise = gameLoop game
            where renderVictory game player = do
                    putStr $ playerToString player ++ " has won the game.\n"
                    renderBoard game
                    return 1

prompt msg = do
  putStr msg
  hFlush stdout
  getLine

main = gameLoop (Game makeNewBoard initialPlayer [])
