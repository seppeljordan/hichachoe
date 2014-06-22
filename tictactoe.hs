-- We want to program the game of tic tac toe, also known as XO

import Control.Applicative
import System.IO
import Worldstate
import Input
import Output

maybeFallback (Just newVal) _ = newVal
maybeFallback Nothing fallback = fallback

-- Input of Data

checkInput game = do
  putStr $ playerGreeting (show (gameActivePlayer game))
  coordinates <- (getCoordinates "Where? ")
  return (gameProcessMove game coordinates)


playerGreeting p = "It is " ++ show p ++ "s turn!\n"

gameLoop oldGame = do
  render oldGame
  return (gameClearMessages oldGame) >>= checkInput >>= nextTurn
  where nextTurn game 
            | gameWon game /= Nothing = renderVictory game
            | otherwise = gameLoop game
            where renderVictory :: Game -> IO Game
                  renderVictory game = do
                     putStr $ show (gameOtherPlayer game) ++ " has won the game.\n"
                     render game
                     return game

main = gameLoop makeNewGame
