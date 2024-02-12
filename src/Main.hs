module Main where

import EnigmaMachine
import System.IO
import Data.List
import Helpers

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n\n\nWelcome to the Enigma Machine simulator!"
  putStrLn "Please enter a string to encode:\n"
  input <- getLine
  let input' =  map cleanText $ words input
  let machine = defaultsMachine 
  let encoded = (map (encode machine) input')
  putStrLn ("\nEncoded string: " ++ (intercalate " " encoded))


defaultsMachine :: EnigmaMachine
defaultsMachine = 
    let 
     rotor1 = mkRotor "abcdefghijklmnopqrstuvwxyz" 6
     rotor2 = mkRotor "abcdefghijklmnopqrstuvwxyz" 8 
     rotor3 = mkRotor "abcdefghijklmnopqrstuvwxyz" 10 
     plugboard = mkPlugboard [('a', 'n'),('b', 'o'),('c', 'p'),('d', 'q'),('e', 'r'),('f', 's'),('g', 't'),('h', 'u'),('i', 'v'),('j', 'w'),('k', 'x'),('l', 'y'),('m', 'z')]
     machine = mkEnigmaMachine [rotor1,rotor2,rotor3] plugboard
    in 
      machine