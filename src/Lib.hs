module Lib where

import Data.Array
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Char
import Control.Monad
import Control.Monad.Reader

newtype Trie = Trie { getTrie :: Array Char (Bool, Maybe Trie) }
    deriving (Eq, Show, Ord)

alphabet = ['a'..'z']

runLib :: IO ()
runLib = do
    words <- readFile "data/words.txt"
    let dict = mkTrie . filter (all isLetter) . (map . map) toLower . lines $ words
    putStrLn "Enter valid letters: "
    letters <- getLine
    putStrLn "Enter special letter: "
    (special:_) <- getLine
    putStrLn $ "Finding words with letters \"" ++ letters ++ "\" and special char " ++ show special ++ "..."
    let result = specialWords dict letters special
    let wordsFound = length result
    wordsFound `seq` putStrLn $ "Found " ++ show wordsFound ++ " words."
    sequence_ . map putStrLn $ result
    

emptyTrie :: Trie
emptyTrie = Trie $ array ('a', 'z') (zip alphabet $ repeat (False, Nothing))

mkTrie :: [String] -> Trie
mkTrie = foldl' addWord emptyTrie 

addWord :: Trie -> String -> Trie
addWord t [] = t
addWord (Trie t) (c:cs) = Trie $ t // [(c, (lastLetter, Just childTrie))]
    where lastLetter = length cs == 0 || fst (t ! c)
          (_, maybeChild) = t ! c
          childTrie = case maybeChild of
                         Just child -> addWord child cs 
                         Nothing -> addWord emptyTrie cs 

containsWord :: Trie -> String -> Bool
containsWord _ [] = False 
containsWord (Trie t) (c:[]) = fst $ t ! c 
containsWord (Trie t) (c:cs) = 
    case snd $ t ! c of
         Nothing -> False
         Just child -> containsWord child cs 

specialWords :: Trie -> [Char] -> Char -> [String]
specialWords trie alpha req = filter ((req `elem`) .&&. ((> 3) . length)) $ getWords' trie alpha
    where (.&&.) = liftM2 (&&)

getWords :: Trie -> [String]
getWords = flip getWords' alphabet 

getWords' :: Trie -> [Char] -> [String]
getWords' trie alpha = map reverse $ getWordsHelper trie alpha "" 

getWordsHelper :: Trie -> [Char] -> String -> [String]
getWordsHelper (Trie t) chars word = map (flip (:) word) lastLetters ++ result
    where lastLetters = [l | l <- chars, fst $ t ! l]
          children = catMaybes . fmap sequence . (fmap . fmap) sequence . assocs $ t
          result = foldl' folder [] children
              where folder words (c, (_, t)) 
                        | c `elem` chars = words ++ getWordsHelper t chars (c:word)
                        | otherwise = words
                                       
          
