module Input ( getCoordinates
             , Coordinates )
where

import Control.Monad
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

coordToNumpad (x,y) = 5 - 3 * y + x 

strToNum :: (Monad m) => String -> m Int
strToNum xs = (liftM addUp.sequence.map charToInt) xs
    where addUp list = foldl (\accu x -> 10* accu + x) 0 list


charToInt :: (Monad m) => Char -> m Int
charToInt '0' = return 0
charToInt '1' = return 1
charToInt '2' = return 2
charToInt '3' = return 3
charToInt '4' = return 4
charToInt '5' = return 5
charToInt '6' = return 6
charToInt '7' = return 7
charToInt '8' = return 8
charToInt '9' = return 9
charToInt x = fail ("charToInt: "++show x++" is not a digit")

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

