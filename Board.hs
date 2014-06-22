-- This module implements the the logic of the game board of a tic tac
-- toe game

module Board ( Board, makeEmptyBoard, boardFieldAt, boardChangeField
             , boardDimensions, boardEmptyAt, boardPlayerAt, boardWon
             , Player , otherPlayer, firstPlayer
             , Coordinates, validCoordinates
             , Field )
where

boardDimensions :: Int
boardDimensions = 3

type Coordinates = (Int,Int)

validCoordinates :: Coordinates -> Bool
validCoordinates (x,y)
    | x < 0 || x > boardDimensions = False
    | y < 0 || y > boardDimensions = False
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

makeNewLine = makeLine $ take boardDimensions $ repeat Empty

instance Show Line where
    show l = concat (map show (getLineList l))

data Board = Board [Line] deriving (Eq)

makeBoardOfList lines = Board lines
boardLines (Board lines) = lines

changeBoardField :: Board -> Coordinates -> Player -> Board
changeBoardField board (x,y) player = makeBoardOfList $ iterateBoard (boardLines board) y
    where iterateBoard (line:lines) 0 = (lineSetFieldAt line x newValue):lines
          iterateBoard (line:lines) n = line:(iterateBoard lines (n - 1))
          newValue = playerToField player

boardChangeField = changeBoardField

boardEmptyAt :: Board -> Coordinates -> Bool
boardEmptyAt b (x,y)
    | not (validCoordinates (x,y)) = error $ "boardEmptyAt: invalid Coordinates " ++ show (x,y)
    | boardFieldAt b (x,y) == Empty = True
    | otherwise = False

makeEmptyBoard :: Board
makeEmptyBoard = makeBoardOfList (take boardDimensions $ repeat makeNewLine)


instance Show Board where
    show b = unlines $ map show $ map getLineList (boardLines b)

boardFieldAt :: Board -> Coordinates -> Field
boardFieldAt board (x,y)
    | validCoordinates (x,y) = (lineFieldAt (line y) x)
    | otherwise = error $ "boardFieldAt: invalid coordinates " ++ show (x,y)
    where line n = (boardLines board) !! n

boardPlayerAt :: Board -> Coordinates -> Maybe Player
boardPlayerAt b (x,y)
    | boardFieldAt b (x,y) == Empty = Nothing
    | otherwise = Just $ fieldToPlayer $ boardFieldAt b (x,y)

boardWon :: Board -> Maybe Player
boardWon b
    | playerWon PlayerOne && playerWon PlayerTwo = error $ "Invalid game situation:\n" ++ show b
    | playerWon PlayerOne = Just PlayerOne
    | playerWon PlayerTwo = Just PlayerTwo
    | otherwise = Nothing
    where boardList :: [Maybe Player]
          boardList = map (boardPlayerAt b) [ (x,y) | y <- [0..boardMaxIndex], x <- [0..boardMaxIndex] ]
          boardMaxIndex :: Int
          boardMaxIndex = boardDimensions - 1
          boardSquare :: [[Maybe Player]]
          boardSquare = let aux [] accu = accu
                            aux xs accu = aux (drop boardDimensions xs) ((take boardDimensions xs):accu)
                        in aux boardList [[]]
          has :: Player -> Int -> (Int -> [Maybe Player]) -> Bool
          has p n line = foldl (&&) True $ map ((Just p) ==) (line n)
          row :: Int -> [Maybe Player]
          row n = boardSquare !! n
          col :: Int -> [Maybe Player]
          col n = [(boardSquare !! n) !! y  | y <- [0..boardMaxIndex]]
          diag :: Int -> [Maybe Player]
          diag 0 = [(boardSquare !! n) !! n | n <- [0..boardMaxIndex]]
          diag 1 = [(boardSquare !! n) !! (boardMaxIndex - n) | n <- [0..boardMaxIndex]]
          playerWon :: Player -> Bool
          playerWon p = or $ [ has p r row | r <- [0..boardMaxIndex]]
                        ++ [ has p r col | r <- [0..boardMaxIndex]]
                        ++ [ has p r diag | r <- [0,1]]

boardToString n [] = ""
boardToString n (b:bs)
    | n == (boardDimensions - 1) = (fieldToString b) ++ "\n" ++ boardToString 0 bs
    | otherwise = (fieldToString b) ++ boardToString (n + 1) bs

data Player = PlayerOne | PlayerTwo | PlayerNone deriving (Eq)

instance Show Player where
    show = playerToString

firstPlayer :: Player
firstPlayer = PlayerOne

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
