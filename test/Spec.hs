{-# LANGUAGE QuasiQuotes #-}

import Data.Text (unpack)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)
import Text.Shakespeare.Text (sbt)

import Text.OTL

main :: IO ()
main = hspec $ do
  describe "Text.OTL.parse" $ do
    it "parses a one-line outline" $ do
      parse "dummy" "Hello\n" `shouldBe` Right (Outline [Heading "Hello" []])

    let parsed text = do
          let result = parse "dummy" $ unpack text
          result `shouldSatisfy` isRight
          let Right outline = result
          return outline

    it "parses a nontrivial outline" $ do
      Outline items <- parsed $
          [sbt|Plan
              |	Collect underpants
              |	???
              |		:Should probably clarify this step
              |	Profit!
              |]

      map getHeading items `shouldBe` ["Plan"]

      let steps = getHeadingChildren $ head items
      map getHeading steps `shouldBe` ["Collect underpants", "???", "Profit!"]

      let step2 = steps !! 1
      getHeadingChildren step2 `shouldBe` [Body ["Should probably clarify this step\n"]]
