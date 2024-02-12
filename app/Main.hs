module Main where

import EnigmaMachine
import System.IO
import Helpers

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n\n\nWelcome to the Enigma Machine simulator!"
  putStrLn "Please enter a string to encode:\n"
  input <- getLine
  let input' =  cleanText input
  let machine = defaultsMachine 
  let encoded =  (encode machine) input'
  putStrLn ("\nEncoded string: " ++ ( encoded))


defaultsMachine :: EnigmaMachine
defaultsMachine = 
    let 
     rotor1 = mkRotor "abcdefghijklmnopqrstuvwxyz" 6
     rotor2 = mkRotor "abcdefghijklmnopqrstuvwxyz" 8 
     rotor3 = mkRotor "abcdefghijklmnopqrstuvwxyz" 10 
     plugboard = mkPlugboard [('a', 'r'), ('t', 'z'), ('c', 'i'), ('d', 'j'), ('e', 'k'), ('f', 'l')]
     machine = mkEnigmaMachine [rotor1,rotor2,rotor3] plugboard
    in 
      machine