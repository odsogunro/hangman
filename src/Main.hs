-- src/Main.hs

module Main 
    where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

{-
String: the word to be guessed
[Maybe Char]: the characters filled in 
Char: the characters guessed to present

instance Class Data where
	func = 
-}
data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
    show (Puzzle _ discovered guessed) = 
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " guessed so far: " ++ guessed 

allWords :: IO WordList
allWords = do
            dict <- readFile "data/dict.txt"
            return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
             aw <- allWords
             return (filter gameLength aw)
             where gameLength w = let l = length (w :: String) 
                                  in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
                 randomIndex <- randomRIO (0, (-) (length wl) 1)
                 return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

{-
> toLower 'A'
'a'
> all isJust [Just 'a', Just 'b']
True
> all isJust [Just 'a', Nothing]
False

freshPuzzle :: String -> Puzzle
freshPuzzle st = Puzzle str mch ch
-}
freshPuzzle :: String -> Puzzle
freshPuzzle st = Puzzle str mch ch
                 where str = map toLower st
                       mch = map (const Nothing) str
                       ch  = []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) ch = elem ch str

alreadyGueesed ::  Puzzle -> Char -> Bool
alreadyGueesed (Puzzle _ _ guessedChars) ch = elem ch guessedChars

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c)  = c

main :: IO ()
main = do
    putStrLn "hangman game..."
