import Data
import Lib
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a new line" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"
  describe "findWord" $ do
    it "Should find words that exist on the Grid" $ do
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "PERL" `shouldBe` Just "PERL"
    it "Should not find words that don't exist on the Grid" $ do
      findWord grid "HAMSTER" `shouldBe` Nothing
  describe "findWords" $ do
    it "Should find all the words that exist on the Grid" $ do
      findWords grid languages `shouldBe` languages
    it "Should not find words that don't exist on the Grid" $ do
      findWords grid ["FRENCH", "GERMAN", "SPANISH"] `shouldBe` []