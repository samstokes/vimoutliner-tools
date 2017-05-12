{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (zipWithM_)
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
    it "parses a one-line outline" $
      parse "dummy" "Hello\n" `shouldBe` Right (Outline [Heading "Hello" []])

    let parsed text = do
          let result = parse "dummy" $ unpack text
          result `shouldSatisfy` isRight
          let Right outline = result
          return outline

    it "parses an outline into nested headings" $ do
      Outline items <- parsed $
          [sbt|Wishlist
              |	Animals
              |		Pony
              |		1000 Kittens
              |	Food
              |		Infinite chocolate
              |]

      items `shouldBe` [Heading "Wishlist" [
          Heading "Animals" [
            Heading "Pony" []
          , Heading "1000 Kittens" []
          ]
        , Heading "Food" [
            Heading "Infinite chocolate" []
          ]
        ]]

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
      Outline [Heading _ [UserDef mType textlines]] <- parsed $
          [sbt|User-defined block
              |	>MARKDOWN
              |	>Line 1
              |	>Line 2
              |	>
              |	>New para
              |]

      mType `shouldBe` Just "MARKDOWN"
      textlines `shouldBe` ["Line 1", "Line 2", "", "New para"]

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

    it "flattens the nested structure, preserving the nesting level as the header level" $ do
      let outline = Outline [
              Heading "How to be Awesome" [
                  Heading "Step One" [
                      Body ["Be awesome."]
                    ]
                , Heading "Step Two" [
                      Heading "Just kidding!" [
                          Body ["There is no step two."]
                      ]
                    ]
                ]
            ]
      Pandoc _ blocks <- toPandoc StylePresentation outline

      blocks `shouldMeetExpectations` [
          (`shouldBeHeader` (1, "Step One"))
        , (`shouldBeParagraph` "Be awesome.")
        , (`shouldBeHeader` (1, "Step Two"))
        , (`shouldBeHeader` (2, "Just kidding!"))
        , (`shouldBeParagraph` "There is no step two.")
        ]

    it "promotes children of the first toplevel heading" $ do
      let outline = Outline [
              Heading "How to be Awesome" [
                  Heading "A Primer on How to be Awesome" []
                , Heading "About the Author" [
                      Heading "John J Awesome" [
                          Body ["Awesome by name, awesome by nature"]
                        ]
                    ]
                ]
            ]
      Pandoc _ blocks <- toPandoc StylePresentation outline

      blocks `shouldMeetExpectations` [
          (`shouldBeHeader` (1, "A Primer on How to be Awesome"))
        , (`shouldBeHeader` (1, "About the Author"))
        , (`shouldBeHeader` (2, "John J Awesome"))
        , (`shouldBeParagraph` "Awesome by name, awesome by nature")
        ]

    it "turns empty headings into body text, but only if nested 3 or more deep" $ do
      let outline = outlineWith [
              Heading "1" [
                Heading "2" []
              ]
            , Heading "1" [
                Heading "2" [
                  Heading "3" [
                    Heading "4" []
                  ]
                ]
              ]
            ]
      Pandoc _ blocks <- toPandoc StylePresentation outline

      let isHeaderLevel n = (`shouldBeHeader` (n, show n))
      blocks `shouldMeetExpectations` [
          isHeaderLevel 1
        , isHeaderLevel 2
        , isHeaderLevel 1
        , isHeaderLevel 2
        , isHeaderLevel 3
        , (`shouldBeParagraph` "4")
        ]


    itConvertsLeafItems StylePresentation


  describe "Text.OTL.Pandoc.toPandoc StyleNotes" $ do
    it "uses the first toplevel heading as the title" $ do
      Pandoc meta blocks <- toPandoc StyleNotes $ Outline [Heading "Hello" []]

      P.docTitle meta `shouldBe` [P.Str "Hello"]
      length blocks `shouldBe` 0

    it "promotes children of the first toplevel heading" $ do
      let outline = Outline [
              Heading "How to be Awesome" [
                  Heading "A Primer on How to be Awesome" []
                , Heading "About the Author" [
                      Heading "John J Awesome" [
                          Body ["Awesome by name, awesome by nature"]
                        ]
                    ]
                ]
            ]
      Pandoc _ blocks <- toPandoc StyleNotes outline

      blocks `shouldMeetExpectations` [
          (`shouldBeHeader` (2, "A Primer on How to be Awesome"))
        , (`shouldBeHeader` (2, "About the Author"))
        , isBulletList
        ]

    it "renders headers for top-level headings, turns nested headings into nested bullets" $ do
      let outline = Outline [
              Heading "How to be Awesome" [
                  Heading "Step One" [
                      Body ["Be awesome."]
                    ]
                , Heading "Step Two" [
                      Heading "Just kidding!" [
                          Body ["There is no step two."]
                      ]
                    ]
                ]
            ]
      Pandoc _ blocks <- toPandoc StyleNotes outline

      -- headers start at h2 since the template usually renders the title as h1
      blocks `shouldMeetExpectations` [
          (`shouldBeHeader` (2, "Step One"))
        , (`shouldBeBulletList` [[
              (`shouldBeParagraph` "Be awesome.")
            ]])
        , (`shouldBeHeader` (2, "Step Two"))
        , (`shouldBeBulletList` [[
              (`shouldBePlain` "Just kidding!")
            , (`shouldBeBulletList` [[
                  (`shouldBeParagraph` "There is no step two.")
                ]])
            ]])
        ]

    it "renders empty non-toplevel headings as just plain bullets" $ do
      let outline = outlineWith [
              Heading "2" [
                Heading "3" []
              ]
            , Heading "2" []
            ]
      Pandoc _ blocks <- toPandoc StyleNotes outline

      blocks `shouldMeetExpectations` [
          (`shouldBeHeader` (2, "2"))
        , (`shouldBeBulletList` [[(`shouldBePlain` "3")]])
        , (`shouldBeHeader` (2, "2"))
        ]


    it "allows headings and body text to coexist as siblings" $ do
      let outline = outlineWith [
              Heading "Foo bar" [
                  Heading "h" []
                , Body ["body"]
                , Heading "h" []
                ]
            ]
      Pandoc _ (_ : blocks) <- toPandoc StyleNotes outline

      blocks `shouldMeetExpectations` [(`shouldBeBulletList` [
          [(`shouldBePlain` "h")]
        , [(`shouldBeParagraph` "body")]
        , [(`shouldBePlain` "h")]
        ])]


    itConvertsLeafItems StyleNotes


itConvertsLeafItems :: Style -> Spec
itConvertsLeafItems style = do
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


outlineWith :: [Item] -> Outline
outlineWith items = Outline [Heading "dummy title" items]

shouldBeHeader :: P.Block -> (Int, String) -> Expectation
shouldBeHeader (P.Header level _ text) (expLevel, expText) = (level, text) `shouldBe` (expLevel, pandocPlain expText)
shouldBeHeader block (_, text) = expectationFailure $ "expected header " ++ show text ++ ", got " ++ show block

shouldBeCodeBlock :: P.Block -> String -> Expectation
shouldBeCodeBlock (P.CodeBlock _ code) expected = code `shouldBe` expected
shouldBeCodeBlock block _ = expectationFailure $ "not a code block: " ++ show block

shouldHaveTableRows :: P.Block -> [[P.TableCell]] -> Expectation
shouldHaveTableRows (P.Table _ _ _ _ rows) expectedRows = rows `shouldBe` expectedRows
shouldHaveTableRows block _ = expectationFailure $ "not a table: " ++ show block

shouldBePlain :: P.Block -> String -> Expectation
shouldBePlain (P.Plain inlines) expected =
  inlines `shouldBe` pandocPlain expected
shouldBePlain block _ = expectationFailure $ "not plain text: " ++ show block

shouldBeParagraph :: P.Block -> String -> Expectation
shouldBeParagraph (P.Para inlines) expected =
  inlines `shouldBe` pandocPlain expected
shouldBeParagraph block _ = expectationFailure $ "not a paragraph: " ++ show block

isBulletList :: P.Block -> Expectation
isBulletList (P.BulletList _) = return ()
isBulletList block = expectationFailure $ "not a bullet list: " ++ show block

shouldBeBulletList :: P.Block -> [[P.Block -> Expectation]] -> Expectation
shouldBeBulletList (P.BulletList bullets) bulletExpectations = bullets `shouldMeetExpectations` map (flip shouldMeetExpectations) bulletExpectations
shouldBeBulletList block _ = expectationFailure $ "not a bullet list: " ++ show block


shouldMeetExpectations :: [actual] -> [actual -> Expectation] -> Expectation
shouldMeetExpectations actuals expectations = do
    length actuals `shouldBe` length expectations
    zipWithM_ apply actuals expectations
  where apply actual expect = expect actual


pandocPlain :: String -> [P.Inline]
pandocPlain = intersperse P.Space . map P.Str . words
