import Test.Hspec
import qualified Helpers as H

main :: IO ()
main = hspec $ do
  describe "isAlphabetic" $ do
    it "returns True for alphabetic characters" $ do
      H.isAlphabetic 'a' `shouldBe` True
      H.isAlphabetic 'Z' `shouldBe` True

    it "returns False for non-alphabetic characters" $ do
      H.isAlphabetic '1' `shouldBe` False
      H.isAlphabetic '!' `shouldBe` False

  describe "removeAccent" $ do
    it "removes accents from characters" $ do
      H.removeAccent 'á' `shouldBe` 'a'
      H.removeAccent 'õ' `shouldBe` 'o'
      H.removeAccent 'é' `shouldBe` 'e'
      H.removeAccent 'í' `shouldBe` 'i'

    it "leaves non-accented characters unchanged" $ do
      H.removeAccent 'a' `shouldBe` 'a'
      H.removeAccent '1' `shouldBe` '1'

  describe "normalizeChar" $ do
    it "removes accents and converts to lowercase" $ do
      H.normalizeChar 'Á' `shouldBe` 'a'
      H.normalizeChar 'Õ' `shouldBe` 'o'
      H.normalizeChar 'É' `shouldBe` 'e'
      H.normalizeChar 'Í' `shouldBe` 'i'
      H.normalizeChar 'A' `shouldBe` 'a'
      H.normalizeChar '1' `shouldBe` '1'
  
  describe "cleanText" $ do
    it "removes non-alphabetic characters and normalizes the rest" $ do
      H.cleanText "Á1a" `shouldBe` "aa"
      H.cleanText "ãoOó" `shouldBe` "aooo"