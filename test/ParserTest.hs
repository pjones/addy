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
import qualified Data.Attoparsec.Text as Atto
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestData

test :: TestTree
test =
  testGroup
    "Parser"
    [ testCase "isemail tests" testParserWithIsEmail,
      testCase "RFC 5322 examples" testRfc5322Examples,
      testCase "Wikipedia internationalization examples" testWikipediaIntExamples
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

assertParse :: Mode -> Text -> Assertion
assertParse mode text =
  let r = Atto.parseOnly (P.parse mode <* Atto.endOfInput) text
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
      (Either String Email -> Bool) ->
      Mode ->
      IsEmailTest ->
      Assertion
    expect f mode ie =
      let result = runParse mode ie
       in assertBool (report mode ie result) (f result)
    runParse :: Mode -> IsEmailTest -> Either String Email
    runParse mode = Atto.parseOnly (P.parse mode <* Atto.endOfInput) . ietAddr
    report :: Mode -> IsEmailTest -> Either String Email -> String
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
