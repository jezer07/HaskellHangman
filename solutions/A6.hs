module A6 where

import Provided

import Data.List ( intersperse, sort )
import GHC.Generics (S)
import Data.Char (isAlpha)

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
invalidMove = isAlpha

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
setSecret:: IO ()
setSecret = do
        putStr "Enter a secret word:\t"
        
        undefined


-- *** A6-1: Records & Instances *** --

-- Q#08
data Game

-- Q#09

repeatedMove = undefined

-- Q#10

makeGame = undefined

-- Q#11

updateGame = undefined

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe = undefined

-- Q#15

validateSecret = undefined

-- Q#16

hasValidChars = undefined


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined