module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State

-- *** A8: Monads *** --

-- Q#01
validateNoDict:: Secret -> Either GameException Secret 
validateNoDict s = hasValidChars s >>= isValidLength 

validateWithDict::  Dictionary -> Secret -> Either GameException Secret 
validateWithDict d s = validateNoDict s >>= isInDict d

-- Q#02
playGame:: Game -> IO ()
playGame game = do
    promptGuess
    move <- getUpperChar
    _SPACE_
    case processTurn move game of 
        Left GameOver -> print GameOver >> putStr "The secret word is: "  >> putStrLn (getSecret game)
        Left err -> print err >> playGame game
        Right x -> do
            print x
            if getSecret x /= getGuess x then playGame x
            else
                putStrLn "You Won!" 

-- Q#03
startGame:: (Secret -> Either GameException Secret) -> IO ()
startGame f = do
        secret <- setSecret
        case makeGameIfValid (f secret) of
          Left err -> print err >> startGame f
          Right g -> print g >> playGame g

-- Q#04

runApp :: IO ()
runApp = do
        dict <- getDict
        case dict of 
          Nothing -> do
                 putStrLn "Missing dictionary file! Continue without dictionary? [Y/N]"
                 input <- getUpperChar
                 when (input == 'Y') $ startGame validateNoDict
          Just validator -> startGame $ validateWithDict validator


-- Q#05

makeGameS = undefined


updateGameS = undefined


repeatedMoveS = undefined


processTurnS = undefined