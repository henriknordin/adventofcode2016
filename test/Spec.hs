
import           Test.Hspec

import qualified Advent05 as A05 (advent05, answer1, answer2, validHashes)


main :: IO ()
main = hspec $ do
  let example05 = input05 "abc"
  describe "Advent 5-1" $
    context "For the seed" $
      it "'abc' it produces the password '18f47a30'" $
        A05.answer1 example05 `shouldBe` "18f47a30"
  describe "Advent 5-2" $
    context "For the seed" $
      it "'abc' it produces the password '05ace8e3'" $
        A05.answer2 example05 `shouldBe` "05ace8e3"

input05 :: String -> [String]
input05 = A05.validHashes
