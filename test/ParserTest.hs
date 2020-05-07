-- |
--
-- Copyright:
--   This file is part of the package addy. It is subject to the license
--   terms in the LICENSE file found in the top-level directory of this
--   distribution and at:
--
--     https://code.devalot.com/open/addy
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module ParserTest
  ( test,
  )
where

import Addy.Internal.Parser as P
import Addy.Internal.Types
import qualified Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestData

test :: TestTree
test =
  testGroup
    "Parser"
    [ testCase "isemail tests" testParserWithIsEmail,
      testCase "RFC 5322 examples" testRfc5322Examples,
      testCase "Wikipedia internationalization examples" testWikipediaIntExamples,
      testCase "Miscellaneous examples" testMiscExamples,
      testProperty
        "Generated short addresses"
        (testGeneratedExamples genShortEmail),
      testProperty
        "Generated long addresses"
        (testGeneratedExamples genLongEmail)
    ]

testRfc5322Examples :: Assertion
testRfc5322Examples = do
  mapM_ (fst >>> go) rfc5322Examples
  -- Only works in lenient mode:
  mapM_ (\(a, _, _) -> assertParse Lenient a) rfc5322ObsExamples
  where
    go :: Text -> Assertion
    go t = do
      assertParse Strict t
      assertParse Lenient t

testWikipediaIntExamples :: Assertion
testWikipediaIntExamples =
  mapM_ go wikipediaIntExamples
  where
    go t = do
      assertParse Strict t
      assertParse Lenient t

testMiscExamples :: Assertion
testMiscExamples =
  forM_ examples $ \t -> do
    assertParse Strict t
    assertParse Lenient t
  where
    examples =
      miscExamples
        <> [ "(((((((((())))))))))user@example.org"
           ]

testGeneratedExamples :: Hedgehog.Gen Text -> Hedgehog.Property
testGeneratedExamples gen =
  Hedgehog.property $ do
    source <- Hedgehog.forAll gen
    let result = runParse source
    Hedgehog.annotateShow result
    Hedgehog.assert (isRight result)
  where
    runParse = parseWithMode Strict

assertParse :: Mode -> Text -> Assertion
assertParse mode text =
  let r = parseWithMode mode text
   in assertBool
        (toString text <> " " <> show r <> " " <> show mode)
        (isRight r)

testParserWithIsEmail :: Assertion
testParserWithIsEmail = mapM_ go =<< isEmailTests
  where
    go :: (IsEmailTest, IsEmailCat) -> Assertion
    go (isemail, cat) = case cat of
      CatError -> do
        shouldFail Strict isemail
        shouldFail Lenient isemail
      CatDeprec -> do
        shouldFail Strict isemail
        shouldPass Lenient isemail
      CatOkay ->
        shouldPass Strict isemail
    shouldPass, shouldFail :: Mode -> IsEmailTest -> Assertion
    shouldPass = expect isRight
    shouldFail = expect isLeft
    expect ::
      (Either (NonEmpty Error) EmailAddr -> Bool) ->
      Mode ->
      IsEmailTest ->
      Assertion
    expect f mode ie =
      let result = runParse mode ie
       in assertBool (report mode ie result) (f result)
    runParse :: Mode -> IsEmailTest -> Either (NonEmpty Error) EmailAddr
    runParse mode = parseWithMode mode . ietAddr
    report :: Mode -> IsEmailTest -> Either (NonEmpty Error) EmailAddr -> String
    report mode isemail result =
      mconcat
        [ "IsEmail test id ",
          show (ietId isemail),
          " in mode: ",
          show mode,
          " did not expect: ",
          show result,
          " from: ",
          toString (ietAddr isemail)
        ]
