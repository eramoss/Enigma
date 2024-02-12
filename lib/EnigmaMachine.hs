module EnigmaMachine (encode,mkEnigmaMachine, mkPlugboard,mkRotor ,EnigmaMachine, rotateRotors, applyPlugboard, rotors,position, rotateRotor, passThroughRotor, inversePassThroughRotor,passThroughRotors) where

import Data.Map (Map)
import qualified Data.Map as Map 
import qualified Helpers as H
import Debug.Trace

data Rotor = Rotor {
  wiring :: String,  -- The wiring configuration of the rotor
  position :: Int   -- The current position of the rotor
}deriving Show

mkRotor :: String -> Int -> Rotor
mkRotor wiring position = Rotor wiring (position `mod` 26)

data Plugboard = Plugboard {
  connections :: Map Char Char
} deriving Show

mkPlugboard :: [(Char, Char)] -> Plugboard
mkPlugboard connections = Plugboard (Map.fromList connections)

data EnigmaMachine = EnigmaMachine {
  rotors :: [Rotor],      -- The rotors in the machine, the last rotor is a loopback
  plugboard :: Plugboard 
} deriving Show

mkEnigmaMachine :: [Rotor] -> Plugboard -> EnigmaMachine
mkEnigmaMachine rotors plugboard = EnigmaMachine rotors plugboard

encode :: EnigmaMachine -> String -> String
encode _ ([]) = []
encode machine (char: input) = 
  let
    char' = applyPlugboard (plugboard machine) char
    machine' = rotateRotors machine
    char'' = passThroughRotors machine' char'
    char''' = applyPlugboard (plugboard machine') char''
  in 
    char''' : encode machine' input


passThroughRotors :: EnigmaMachine -> Char -> Char
passThroughRotors machine@(EnigmaMachine { rotors = rs }) c = 
  let
    c' = passThroughRotor (rs !! 0) c
    c'' = passThroughRotor (rs !! 1) c'
    c''' = passThroughRotor (rs !! 2) c''

    cr' = inversePassThroughRotor (rs !! 2) c'''
    cr'' = inversePassThroughRotor (rs !! 1) cr'
    cr''' = inversePassThroughRotor (rs !! 1) cr''

  in
    cr'''


passThroughRotor :: Rotor -> Char -> Char
passThroughRotor (Rotor rWiring rPosition) c = 
  let
    index = (rPosition + (fromEnum c + fromEnum 'A')) `mod` 26
    c' = rWiring !! index
  in
    c'
inversePassThroughRotor :: Rotor -> Char -> Char
inversePassThroughRotor (Rotor rWiring rPosition) c = 
  let
    index = (rPosition - (fromEnum c + fromEnum 'A')) `mod` 26
    c' = rWiring !! index
  in
    c'

applyPlugboard :: Plugboard -> Char -> Char
applyPlugboard (Plugboard pbConnections) c = 
  case Map.lookup c pbConnections of
    Just c' -> c'  -- Found c as a key
    Nothing -> 
      case H.findKey c pbConnections of
        Just c' -> c'  -- Found c as a value
        Nothing -> c   -- Didn't find c as a key or a value

rotateRotors :: EnigmaMachine -> EnigmaMachine
rotateRotors machine@(EnigmaMachine { rotors = rs }) 
  | (position (rs !! 0)) `mod` 26 /= 0 = machine {rotors = [rotateRotor (rs !! 0), rs !! 1, rs !! 2]}
  | (position (rs !! 1)) `mod` 26 /= 0 = machine {rotors = [rotateRotor (rs !! 0), rotateRotor (rs !! 1), rs !! 2]}
  | otherwise = machine {rotors = [rotateRotor (rs !! 0), rotateRotor (rs !! 1), rotateRotor (rs !! 2)]}

rotateRotor :: Rotor -> Rotor
rotateRotor (Rotor rWiring rPosition) =
  let
    new_position = (rPosition `mod` 26) + 1
  in
    Rotor rWiring new_position

