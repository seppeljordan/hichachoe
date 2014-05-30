-- We want to program the game of tic tac toe, also known as XO

import Control.Applicative
import System.IO

lineLength = 3
initialPlayer = PlayerOne

maybeFallback (Just newVal) _ = newVal
maybeFallback Nothing fallback = fallback

data Field = Empty | X | O deriving (Show, Eq)

fieldToString Empty = "."
fieldToString X = "X"
fieldToString O = "O"

fieldToPlayer X = PlayerOne
fieldToPlayer O = PlayerTwo
fieldToPlayer Empty = PlayerNone

type Board = [Field]

fieldAt board x y
    | validCoord x && validCoord y = Just (board !! coordinatesToIndex x y)
    | otherwise = Nothing

makeNewBoard = take (lineLength * lineLength) $ repeat Empty

setField board field (x,y)
    | x < 0 || x > lineLength = Nothing
    | y < 0 || y > lineLength = Nothing
    | fieldAt board x y /= Just Empty = Nothing
    | otherwise = setFieldByIndex board field (coordinatesToIndex x y)

setFieldByIndex _ _ n
    | n < 0 || n > (lineLength * lineLength) = Nothing
setFieldByIndex (b:bs) field 0 = Just (field:bs)
setFieldByIndex (b:bs) field n = (Just (b:)) <*> (setFieldByIndex bs field (n - 1))

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

type Message = String

renderMessage m = putStr (m ++ "\n")
renderMessages (Game board player []) = return (Game board player [])
renderMessages (Game board player (m:ms)) = do
  renderMessage m
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
                    isOfPlayer x y = if (player == (maybe PlayerNone fieldToPlayer (fieldAt board x y))) then True else False
                              
                               

coordinatesToIndex x y = x + ( lineLength *y)

validCoord x
    | x < 0 = False
    | x > 2 = False
    | otherwise = True

boardToString n [] = ""
boardToString n (b:bs)
    | n == (lineLength - 1) = (fieldToString b) ++ "\n" ++ boardToString 0 bs
    | otherwise = (fieldToString b) ++ boardToString (n + 1) bs

renderBoard g = do
  putStr $ boardToString 0 (getGameBoard g)
  return g

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
    where newboard = maybeFallback (setField board (playerToField player) coordinates) board
          newplayer = if turnfail then player else otherPlayer player
          turnfail = (newboard == board)
          turnsuccess = not turnfail
          newmessages = if turnsuccess then messages else "Invalid Turn":messages

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
