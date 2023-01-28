{-# LANGUAGE InstanceSigs #-}
module A6 where

import Provided

import Data.List ( intersperse, sort )
import GHC.Generics (S)
import Data.Char (isAlpha, toUpper, toLower)

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int 
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

         
-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

-- Q#03
lengthInRange:: Secret -> Bool
lengthInRange s = let min = fst _LENGTH_  
                      max = snd _LENGTH_ in 
                      length s >=min && length s <= max


-- Q#04
invalidMove:: Move -> Bool
invalidMove = not . isAlpha

-- Q#05
revealLetters:: Move -> Secret -> Guess -> Guess
revealLetters m s g = zipWith (\s' g' -> if m == s' then s' else g') s g 

-- Q#06
updateChances:: Move -> Secret -> Chances -> Chances
updateChances m s c = if found False s then c else c-1
      where
        found acc [] = acc
        found acc (x:xs) = m == x || found False xs

-- Q#07
setSecret:: IO String
setSecret = do
        putStr "Enter a secret word:\t"
        showInput False
        secret <- getLine
        showInput True
        _SPACE_
        return secret


-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game {
                getSecret:: String,
                getGuess:: String,
                getMoves:: [Char],
                getChances:: Int
            }

-- Q#09
repeatedMove:: Move -> Game -> Bool
repeatedMove m g = m `elem` getMoves g

-- Q#10
makeGame:: Secret -> Game
makeGame secret = Game {
                        getSecret = map toUpper secret,
                        getGuess = replicate (length secret) '_',
                        getMoves = [],
                        getChances = _CHANCES_
                }

-- Q#11
updateGame:: Move -> Game -> Game
updateGame m g = let guess = revealLetters m (getSecret g) (getGuess g) 
                  
                 in g {
                        getGuess = guess,
                        getMoves = m: getMoves g,
                        getChances = updateChances m (getSecret g) (getChances g)  
                 }

-- Q#12
instance Show Game where
        show g = showGameHelper (getSecret g) (getMoves g) (getChances g)

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13
instance Show GameException where
        show :: GameException -> String
        show g = case g of 
                 InvalidWord -> concat ["Invalid Word. It should be all letters and the minimum allowed charcaters is ", lb, ", and maximum ",ub] 
                 InvalidMove -> "Invalid Move"
                 RepeatMove -> "Repeat Move"
                 GameOver -> "Game Over"
                 where 
                   lb = show $ fst _LENGTH_
                   ub = show $ snd _LENGTH_

-- *** A6-2: Exception Contexts *** --

-- Q#14
toMaybe:: Bool -> a -> Maybe a
toMaybe b a = if b then Just a else Nothing

-- Q#15
validateSecret:: (Secret -> Bool) -> GameException-> Secret -> Either GameException Secret 
validateSecret f g s = if f s then Right s else Left g

-- Q#16
hasValidChars::  Secret -> Either GameException Secret 
hasValidChars = validateSecret (foldr (\e acc -> isAlpha e && acc) True) InvalidWord

isValidLength::  Secret -> Either GameException Secret 
isValidLength = validateSecret lengthInRange InvalidWord

isInDict::  Dictionary -> Secret -> Either GameException Secret 
isInDict dict = validateSecret (\xs -> map toLower xs `elem` dict) InvalidWord

-- Q#17
validateNoDict:: Secret -> Either GameException Secret 
validateNoDict s = case hasValidChars s of
                Right s -> isValidLength s
                Left err -> Left err

validateWithDict::  Dictionary -> Secret -> Either GameException Secret 
validateWithDict d s = case validateNoDict s of
                Right s -> isInDict d s
                Left err -> Left err

-- Q#18
processTurn:: Move -> Game -> Either GameException Game
processTurn m g 
        | invalidMove m = Left InvalidMove
        | repeatedMove m g = Left RepeatMove
        | otherwise = updatedGame
        where 
           updatedGame = if getChances (updateGame m g) < 1 then Left GameOver else Right g