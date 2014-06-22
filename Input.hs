module Input ( getCoordinates
             , Coordinates )
where

import Control.Applicative
import Worldstate
import System.IO

inputBindsTo :: String -> Coordinates -> (String -> Maybe Coordinates)
[] `inputBindTo` _ = error "inputBindsTo: tried to bind empty string to coordinates"
str `inputBindsTo` coords@(x,y)
    | not (validCoordinates (x,y)) = error $ "inputBindsTo: tried to bind input to invalid coodinates " ++ show coords
    | otherwise = \x -> if x == str then Just coords else Nothing

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

getCoordinates :: String -> IO Coordinates
getCoordinates inputprompt = do
  input <- prompt inputprompt
  result $ coordinates input
  where coordinates x = strToNum x >>= numpadToCoord
        result Nothing = getCoordinates inputprompt
        result (Just x) = return x

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

