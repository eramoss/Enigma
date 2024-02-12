{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified EnigmaMachine as E
import Data.Map (Map)
import qualified Data.Map as Map 

main :: IO ()
main = hspec $ do
  describe "rotate rotors" $ do
    it "should be rotated 1 by 1 until 26" $ do --
      let machine = defaultsMachine
      let machine' = rotateRotorsHelp machine "asdfjetrospdfgaherthfksd2f"
      let rotors = E.rotors machine'
      let positions = map E.position rotors
      positions `shouldBe` [1,2,1]
  describe "apply plugboard on chars" $ do
    it "should return the correct char" $ do
      let plugboard = E.mkPlugboard [('A', 'G'),('B', 'H'),('C', 'I'),('D', 'J'),('E', 'K'),('F', 'L')]
      let char = E.applyPlugboard plugboard 'A'
      char `shouldBe` 'G'
    it "should search for inverse plug" $ do
      let plugboard = E.mkPlugboard [('A', 'G'),('B', 'H'),('C', 'I'),('D', 'J'),('E', 'K'),('F', 'L')]
      let char = E.applyPlugboard plugboard 'G'
      char `shouldBe` 'A'
  describe "pass through rotor" $ do
    it "should return the 1 char forward" $ do
      let rotor = E.mkRotor "ABCDEFGHIJKLMNOPQRSTUVXWYZ" 1
      let char = E.passThroughRotor rotor 'A'
      char `shouldBe` 'B'
    it "should return the 1 char backward" $ do
      let rotor = E.mkRotor "ABCDEFGHIJKLMNOPQRSTUVXWYZ" 1
      let char = E.inversePassThroughRotor rotor 'B'
      char `shouldBe` 'A'
  describe "pass through rotors" $ do
    it "should encode the char through 6 rotors" $ do
      let machine = defaultsMachine
      let char = E.passThroughRotors machine 'A'
      char `shouldBe` 'Y'
    it "should encode and decode" $ do
      let machine = defaultsMachine
      let char = E.passThroughRotors machine 'S'
      let char' = E.passThroughRotors machine char
      char' `shouldBe` 'S'
  describe "encode" $ do
    it "should encode the string" $ do
      let machine = defaultsMachine
      let string = E.encode machine "HELLO"
      string `shouldBe` "RUNNK"
    it "should encode and decode" $ do
      let machine = defaultsMachine
      let string = E.encode machine "HELLO"
      let string' = E.encode machine string
      string' `shouldBe` "HELLO"

rotateRotorsHelp :: E.EnigmaMachine -> String -> E.EnigmaMachine
rotateRotorsHelp machine input =
  foldl (\m _ -> E.rotateRotors m) machine input

defaultsMachine :: E.EnigmaMachine
defaultsMachine = 
    let 
     rotor1 = E.mkRotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1
     rotor2 = E.mkRotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1 
     rotor3 = E.mkRotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 1 
     plugboard = E.mkPlugboard [('A', 'N'),('B', 'O'),('C', 'P'),('D', 'Q'),('E', 'R'),('F', 'S'),('G', 'T'),('H', 'U'),('I', 'V'),('J', 'W'),('K', 'X'),('L', 'Y'),('M', 'Z')]
     machine = E.mkEnigmaMachine [rotor1,rotor2,rotor3] plugboard
    in 
      machine