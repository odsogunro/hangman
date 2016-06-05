-- src/Main.hs

module Main 
    where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

--type WordList = [String]
newtype WordList = WordList [String]
    deriving (Eq, Show)

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
            return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
             (WordList aw) <- allWords
             return $ WordList (filter gameLength aw)
             where gameLength w = let l = length (w :: String) 
                                  in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
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
                 where str = fmap toLower st
                       mch = fmap (const Nothing) str
                       ch  = []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) ch = elem ch str

alreadyGuessed ::  Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gCh) ch = elem ch gCh

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c)  = c
renderPuzzleChar Nothing = '_'


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = 
    Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar = 
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = 
            zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
    putStrLn $ "your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of 
        (_, True) -> do 
            putStrLn "you already guessed that character, pick something else!"
            return puzzle

        (True, _) -> do 
            putStrLn "this character was in the word, filling in the word accordingly."
            return (fillInCharacter puzzle guess)

        (False, _) -> do 
            putStrLn "this character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)

maxNumberOfGuessesAllowed :: Int
maxNumberOfGuessesAllowed = 7

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
    if (length guessed) > maxNumberOfGuessesAllowed
    then do putStrLn "you lose!"
            putStrLn $ "the word was: " ++ wordToGuess
            exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = 
    if all isJust filledInSoFar
    then do putStrLn "you win!"
            exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "current puzzle is: " ++ show puzzle
    putStr "guess a letter: "
    guess <- getLine
    case guess of 
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "your guess must be single character."

main :: IO ()
main = do
    putStrLn "hangman game..."
    word <- randomWord'
    let puzzle = freshPuzzle word
    runGame puzzle

