{-# LANGUAGE QuasiQuotes #-}

import Data.Text (unpack)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (isRight)
import Text.Shakespeare.Text (sbt)

import Text.Pandoc (Pandoc(..))
import qualified Text.Pandoc as P

import Text.OTL
import Text.OTL.Pandoc

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

  describe "Text.OTL.Pandoc.toPandoc StylePresentation" $ do
    it "uses the first toplevel heading as the title" $ do
      Pandoc meta blocks <- toPandoc StylePresentation $ Outline [Heading "Hello" []]

      P.docTitle meta `shouldBe` [P.Str "Hello"]
      length blocks `shouldBe` 0

    let outlineWith items = Outline [Heading "dummy title" items]

    it "converts a table" $ do
      let table = Table [TableRow False ["John", "Doe"]]
          outline = outlineWith [table]
      Pandoc _ blocks <- toPandoc StylePresentation outline

      length blocks `shouldBe` 1
      let ptable = head blocks

      let shouldHaveTableRows (P.Table _ _ _ _ rows) expectedRows = rows `shouldBe` expectedRows
          shouldHaveTableRows block _ = expectationFailure $ "not a table: " ++ show block
          row = [[P.Plain [P.Str "John"]], [P.Plain [P.Str "Doe"]]]
      ptable `shouldHaveTableRows` [row]
