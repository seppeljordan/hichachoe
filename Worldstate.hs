module Worldstate ( gameProcessMove, gameActivePlayer, gameOtherPlayer
                  , makeNewGame
                  , gameWon
                  , gameAddMessage, gameClearMessages
                  , Message, makeMessage
                  , Player
                  , Coordinates, boardDimensions, validCoordinates )
where

import Board
import Output

data Message = Message String

getMessage (Message m) = m
makeMessage m = Message m

instance Show Message where
    show m = getMessage m

instance Renderable Message where
    render m = putStr $ show m ++ "\n"

data Game = Game Board Player [Message] deriving (Show)

makeGame :: Board -> Player -> [Message] -> Game
makeGame b p ms = Game b p ms

gamePlayer (Game _ player _) = player
gameBoard (Game board _ _) = board
gameMessages (Game _ _ messages) = messages

gameActivePlayer :: Game -> Player
gameActivePlayer game = gamePlayer game

gameOtherPlayer :: Game -> Player
gameOtherPlayer game = otherPlayer $ gamePlayer game

gameAddMessage :: Game -> Message -> Game
gameAddMessage game m = makeGame (gameBoard game) (gamePlayer game) (m: (gameMessages game))

gameClearMessages :: Game -> Game
gameClearMessages game = makeGame (gameBoard game) (gamePlayer game) []

makeNewGame :: Game
makeNewGame = Game makeEmptyBoard firstPlayer []

gameChangePlayer :: Game -> Game
gameChangePlayer game = makeGame oldBoard newPlayer oldMessages
    where oldBoard = gameBoard game
          newPlayer = otherPlayer $ gamePlayer game
          oldMessages = gameMessages game

gameChangeBoard :: Game -> Board -> Game
gameChangeBoard g b = Game b oldPlayer oldMessages
    where oldPlayer = gamePlayer g
          oldMessages = gameMessages g
                     
gameProcessMove :: Game -> Coordinates -> Game
gameProcessMove game coords = if validTurn
                              then makeGame newBoard newPlayer oldMessages
                              else makeGame oldBoard oldPlayer newMessages
    where validTurn = boardEmptyAt oldBoard coords
          newBoard = boardChangeField oldBoard coords oldPlayer
          newPlayer = otherPlayer oldPlayer
          newMessages = (makeMessage ("Invalid Turn, Field already occupied")):oldMessages
          oldBoard = gameBoard game
          oldPlayer = gamePlayer game
          oldMessages = gameMessages game

gameWon :: Game -> Maybe Player
gameWon game = boardWon (gameBoard game)

coordinatesToIndex x y = x + ( boardDimensions *y)

validCoord x
    | x < boardDimensions = False
    | x > boardDimensions = False
    | otherwise = True

instance Renderable Game where
    render game = do
      render $ gameBoard game
      sequence_ $ map render (reverse (gameMessages game))
