{-# LANGUAGE QuasiQuotes #-}

import Data.Text (unpack)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)
import Text.Shakespeare.Text (st)

import Text.OTL

main :: IO ()
main = hspec $ do
  describe "Text.OTL.parse" $ do
    let parsed = parse "dummy"

    it "parses a one-line outline" $ do
      parsed "Hello\n" `shouldBe` Right (Outline [Heading "Hello" []])

    it "parses a nontrivial outline" $ do
      let doc = unpack [st|Plan
	Collect underpants
	???
		:Should probably clarify this step
	Profit!
      |]

      let result = parsed doc
      result `shouldSatisfy` isRight
      let Right (Outline items) = result

      map getHeading items `shouldBe` ["Plan"]

      let steps = getHeadingChildren $ head items
      map getHeading steps `shouldBe` ["Collect underpants", "???", "Profit!"]

      let step2 = steps !! 1
      getHeadingChildren step2 `shouldBe` [Body ["Should probably clarify this step\n"]]
