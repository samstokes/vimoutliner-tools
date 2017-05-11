{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (mapM_)
import Data.List (intersperse)
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

    it "parses an outline with multiple top-level items" $ do
      Outline items <- parsed $
          [sbt|Awesome Presentation
              |	By an awesome person
              |Slide 1
              |	what's that coming over the hill
              |Slide 2
              |	is it a monster
              |]

      map getHeading items `shouldBe` ["Awesome Presentation", "Slide 1", "Slide 2"]

    it "parses a body block into paragraphs" $ do
      Outline [Heading _ [Body paras]] <- parsed $
          [sbt|Body block
              |	:Line 1
              |	:Line 2
              |	:
              |	:New para
              |]

      paras `shouldBe` ["Line 1\nLine 2\n", "New para\n"]

    it "parses a preformatted block into a single string" $ do
      Outline [Heading _ [Preformatted content]] <- parsed $
          [sbt|Preformatted block
              |	;for i in *
              |	;do
              |	;
              |	;  echo $i
              |	;
              |	;done
              |]

      content `shouldBe` "for i in *\ndo\n\n  echo $i\n\ndone\n"

    -- TODO is this really the right semantics? (vs paras or a single string)
    it "parses a user-defined block into lines" $ do
      Outline [Heading _ [UserDef mType lines]] <- parsed $
          [sbt|User-defined block
              |	>MARKDOWN
              |	>Line 1
              |	>Line 2
              |	>
              |	>New para
              |]

      mType `shouldBe` Just "MARKDOWN"
      lines `shouldBe` ["Line 1", "Line 2", "", "New para"]

    it "parses a user-defined preformatted block into a single string" $ do
      Outline [Heading _ [PreUserDef mType content]] <- parsed $
          [sbt|User-defined preformatted block
              |	<BASH
              |	<for i in *
              |	<do
              |	<
              |	<  echo $i
              |	<
              |	<done
              |]

      mType `shouldBe` Just "BASH"
      content `shouldBe` "for i in *\ndo\n\n  echo $i\n\ndone\n"

    it "parses a table without a header" $ do
      Outline [Heading _ [Table rows]] <- parsed $
          [sbt|Table
              |	|Frodo  |Hobbit|
              |	|Gandalf|Wizard|
              |	|Aragorn|Man   |
              |]

      length rows `shouldBe` 3

      mapM_ (`shouldSatisfy` not.isRowHeader) rows

      let names = map (head . getRowEntries) rows
      -- TODO would be nice if it stripped off the trailing whitespace...
      names `shouldBe` ["Frodo  ", "Gandalf", "Aragorn"]

    it "parses a table with a header" $ do
      Outline [Heading _ [Table rows]] <- parsed $
          [sbt|Table
              |	|| First name | Last name |
              |	|  John       | Major     |
              |	|  Tony       | Blair     |
              |]

      length rows `shouldBe` 3
      let header : entries = rows

      header `shouldSatisfy` isRowHeader

      let lastNames = map ((!! 1) . getRowEntries) entries
      -- TODO would be nice if it stripped off the trailing whitespace...
      lastNames `shouldBe` ["Major     ", "Blair     "]


  describe "Text.OTL.Pandoc.toPandoc StylePresentation" $ do
    it "uses the first toplevel heading as the title" $ do
      Pandoc meta blocks <- toPandoc StylePresentation $ Outline [Heading "Hello" []]

      P.docTitle meta `shouldBe` [P.Str "Hello"]
      length blocks `shouldBe` 0

    itConvertsLeafItems StylePresentation


  describe "Text.OTL.Pandoc.toPandoc StyleNotes" $ do
    it "uses the first toplevel heading as the title" $ do
      Pandoc meta blocks <- toPandoc StyleNotes $ Outline [Heading "Hello" []]

      P.docTitle meta `shouldBe` [P.Str "Hello"]
      length blocks `shouldBe` 0

    itConvertsLeafItems StyleNotes


itConvertsLeafItems :: Style -> Spec
itConvertsLeafItems style = do
  let outlineWith items = Outline [Heading "dummy title" items]

  it "converts a table" $ do
    let table = Table [TableRow False ["John", "Doe"]]
        outline = outlineWith [table]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let ptable = head blocks

    let row = [[P.Plain [P.Str "John"]], [P.Plain [P.Str "Doe"]]]
    ptable `shouldHaveTableRows` [row]

  it "converts preformatted content" $ do
    let smiley = unpack [sbt|^   ^
                            |  o
                            |\___/
                            |]
        outline = outlineWith [Preformatted smiley]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let code = head blocks

    code `shouldBeCodeBlock` smiley

  it "converts body text" $ do
    let paras = ["Hello world", "OHAI"]
        outline = outlineWith [Body paras]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 2

    mapM_ (uncurry shouldBeParagraph) (zip blocks paras)

  it "renders markdown in a user-defined block" $ do
    let markdown = UserDef (Just "markdown") ["foo *bar* baz"]
        outline = outlineWith [markdown]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let para = head blocks

    para `shouldBe` P.Para [P.Str "foo", P.Space, P.Emph [P.Str "bar"], P.Space, P.Str "baz"]

  -- TODO "almost-normal" - renders soft breaks instead of separate para blocks
  it "renders almost-normal body text for a user-defined block with unrecognised type" $ do
    let paras = ["Saluton mondo", "OHAI"]
        outline = outlineWith [UserDef (Just "esperanto") paras]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let para = head blocks

    para `shouldBe` P.Para [
        P.Str "Saluton", P.Space, P.Str "mondo", P.SoftBreak
      , P.Str "OHAI", P.SoftBreak
      ]

  -- TODO "almost-normal" - renders soft breaks instead of separate para blocks
  it "renders almost-normal body text for a user-defined block with no type" $ do
    let paras = ["O RLY", "YA RLY"]
        outline = outlineWith [UserDef Nothing paras]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let para = head blocks

    para `shouldBe` P.Para [
        P.Str "O", P.Space, P.Str "RLY", P.SoftBreak
      , P.Str "YA", P.Space, P.Str "RLY", P.SoftBreak
      ]

  it "renders annotated code blocks for user-defined preformatted blocks" $ do
    let bash = PreUserDef (Just "bash") "echo $PATH"
        outline = outlineWith [bash]
    Pandoc _ blocks <- toPandoc style outline

    length blocks `shouldBe` 1
    let code = head blocks

    code `shouldBeCodeBlock` "echo $PATH"
    let P.CodeBlock (_, types, _) _ = code
    types `shouldBe` ["bash"]


shouldBeCodeBlock :: P.Block -> String -> Expectation
shouldBeCodeBlock (P.CodeBlock _ code) expected = code `shouldBe` expected
shouldBeCodeBlock block _ = expectationFailure $ "not a code block: " ++ show block

shouldHaveTableRows :: P.Block -> [[P.TableCell]] -> Expectation
shouldHaveTableRows (P.Table _ _ _ _ rows) expectedRows = rows `shouldBe` expectedRows
shouldHaveTableRows block _ = expectationFailure $ "not a table: " ++ show block

shouldBeParagraph :: P.Block -> String -> Expectation
shouldBeParagraph (P.Para inlines) expected =
  inlines `shouldBe` intersperse P.Space (map P.Str $ words expected)
shouldBeParagraph block _ = expectationFailure $ "not a paragraph: " ++ show block
