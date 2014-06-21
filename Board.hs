-- This module implements the the logic of the game board of a tic tac
-- toe game

module Board ( Board, makeEmptyBoard, boardFieldAt
             , Player , otherPlayer
             , Coordinates, validCoordinates
             , Field )
where

lineLength = 3

type Coordinates = (Int,Int)

validCoordinates :: Coordinates -> Bool
validCoordinates (x,y)
    | x < 0 || x > lineLength = False
    | y < 0 || y > lineLength = False
    | otherwise = True

data Field = Empty | X | O deriving (Eq)

fieldToString Empty = "."
fieldToString X = "X"
fieldToString O = "O"

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

instance Show Line where
    show l = concat (map show (getLineList l))

data Board = Board [Line] deriving (Eq)

makeBoardOfList lines = Board lines
boardLines (Board lines) = lines

changeBoardField :: Board -> Coordinates -> Field -> Board
changeBoardField board (x,y) newValue = makeBoardOfList $ iterateBoard (boardLines board) y
    where iterateBoard (line:lines) 0 = (lineSetFieldAt line x newValue):lines
          iterateBoard (line:lines) n = line:(iterateBoard lines (n - 1))

boardChangeField = changeBoardField

boardEmptyAt :: Board -> Coordinates -> Bool
boardEmptyAt b (x,y)
    | not (validCoordinates (x,y)) = error $ "boardEmptyAt: invalid Coordinates " ++ show (x,y)
    | boardFieldAt b (x,y) == Just Empty = True
    | otherwise = False

makeEmptyBoard :: Board
makeEmptyBoard = makeBoardOfList (take lineLength $ repeat makeNewLine)


instance Show Board where
    show b = unlines $ map show $ map getLineList (boardLines b)

boardFieldAt :: Board -> Coordinates -> Maybe Field
boardFieldAt board (x,y)
    | validCoordinates (x,y) = Just (lineFieldAt (line y) x)
    | otherwise = error $ "boardFieldAt: invalid coordinates " ++ show (x,y)
    where line n = (boardLines board) !! n


boardToString n [] = ""
boardToString n (b:bs)
    | n == (lineLength - 1) = (fieldToString b) ++ "\n" ++ boardToString 0 bs
    | otherwise = (fieldToString b) ++ boardToString (n + 1) bs

data Player = PlayerOne | PlayerTwo | PlayerNone deriving (Eq)

instance Show Player where
    show = playerToString

playerToField PlayerOne = X
playerToField PlayerTwo = O
playerToField PlayerNone = Empty

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne
otherPlayer PlayerNone = PlayerNone

playerToString :: Player -> String
playerToString PlayerOne = "Player X"
playerToString PlayerTwo = "Player O"
playerToString PlayerNone = "No Player"
