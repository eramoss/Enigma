module Helpers (findKey , cleanText, normalizeChar, isAlphabetic, removeAccent) where
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.List (find)
import Data.Char (toLower)
import Data.List (nub)

findKey :: (Eq v) => v -> Map k v -> Maybe k
findKey val = fmap fst . find ((== val) . snd) . Map.toList


cleanText :: String -> String
cleanText s = filter isAlphabetic $ map normalizeChar s

-- Normalize characters by removing accents and converting to lowercase
normalizeChar :: Char -> Char
normalizeChar c
    | c `elem` accentedChars_a = removeAccent c
    | c `elem` accentedChars_e = removeAccent c
    | c `elem` accentedChars_i = removeAccent c
    | c `elem` accentedChars_o = removeAccent c
    | otherwise = toLower c

-- Check if a character is alphabetic
isAlphabetic :: Char -> Bool
isAlphabetic ' ' = True
isAlphabetic c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

-- Remove accents from characters
removeAccent :: Char -> Char
removeAccent c
    | c `elem` accentedChars_a = 'a' -- Replace accents with 'a', you may want a different replacement
    | c `elem` accentedChars_o = 'o'
    | c `elem` accentedChars_e = 'e'
    | c `elem` accentedChars_i = 'i'
    | otherwise = c

-- List of accented characters to be replaced

accentedChars_a = "áúàùâÁûäïöüãẽĩũåæøñçßAÁàÀÃ"
accentedChars_o = "õóÒòõôOÒÓÕÔ"
accentedChars_e = "éẽèêEÉÊẼÈ"
accentedChars_i = "íìîĩIÍÌÎĨ"