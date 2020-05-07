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
module RenderTest
  ( test,
  )
where

import qualified Addy.Internal.Parser as Parser
import Addy.Internal.Render
import Addy.Internal.Types
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestData

test :: TestTree
test =
  testGroup
    "Render"
    [ testCase "isemail tests" testFromisEmailTests,
      testCase "RFC 5322 examples" testRfcExamples,
      testCase "Obsolete examples" testObsExamples,
      testCase "Oddities" testOddities,
      testCase "Wikipedia" testWikipediaExamples,
      testCase "Misc" testMiscExamples
    ]

testRfcExamples :: Assertion
testRfcExamples =
  mapM_ go rfc5322Examples
  where
    go :: (Text, Text) -> Assertion
    go (source, expectS) = do
      (_, actualS, actualF) <- roundTrip Parser.Strict source
      let expectF =
            -- We don't render the brackets unless they are needed.
            if Text.isPrefixOf "<" source
              && Text.isSuffixOf ">" source
              then Text.drop 1 (Text.dropEnd 1 source)
              else source
      actualS @?= expectS
      actualF @?= expectF
      idempotentTest actualS actualF

testObsExamples :: Assertion
testObsExamples =
  mapM_ go rfc5322ObsExamples
  where
    go :: (Text, Text, Text) -> Assertion
    go (source, simple, full) = do
      (_, actualS, actualF) <- roundTrip Parser.Lenient source
      actualS @?= simple
      actualF @?= full
      idempotentTest actualS actualF

-- | Weird stuff that we'll parse but won't render.
testOddities :: Assertion
testOddities =
  mapM_
    go
    [ ( "(some (ne(st)ed) comment) example@example.com",
        "(some nested comment) example@example.com",
        "example@example.com"
      )
    ]
  where
    go :: (Text, Text, Text) -> Assertion
    go (source, full, simple) = do
      (_, actualS, actualF) <- roundTrip Parser.Strict source
      actualS @?= simple
      actualF @?= full

testMiscExamples :: Assertion
testMiscExamples =
  forM_ miscExamples $ \source -> do
    (_, actualS, actualF) <- roundTrip Parser.Strict source
    actualS @?= source
    actualF @?= source

testFromisEmailTests :: Assertion
testFromisEmailTests = do
  tests <- isEmailTests
  mapM_
    go
    ( filter
        ( \(iet, cat) ->
            cat == CatOkay
              && ietId iet /= 42 -- Unnecessary quoting.
              && ietId iet /= 45 -- Unnecessary quoting.
              && ietId iet /= 55 -- Unnecessary quoting.
              && ietId iet /= 60 -- Unnecessary quoting.
              && ietId iet /= 79 -- We format better.
              && ietId iet /= 81 -- We format better.
              && ietId iet /= 85 -- We format better.
              && ietId iet /= 88 -- We strip \r\n.
              && ietId iet /= 92 -- We flattten comments.
              && ietId iet /= 144 -- We strip \r\n.
              && ietId iet /= 148 -- We strip \r\n.
              && ietId iet /= 153 -- We strip \r\n.
        )
        tests
    )
  where
    go :: (IsEmailTest, IsEmailCat) -> Assertion
    go (iet, _) = do
      (_, _, actualF) <- roundTrip Parser.Strict (ietAddr iet)
      -- Ignore white space changes
      Text.filter (not . isSpace) actualF
        @?= Text.filter (not . isSpace) (ietAddr iet)

testWikipediaExamples :: Assertion
testWikipediaExamples =
  mapM_ go wikipediaIntExamples
  where
    go :: Text -> Assertion
    go source = do
      (_, simple, full) <- roundTrip Parser.Strict source
      simple @?= source
      full @?= source

roundTrip :: MonadFail m => Parser.Mode -> Text -> m (EmailAddr, Text, Text)
roundTrip pmode t =
  case Parser.parseWithMode pmode t of
    Left e -> fail (show t <> " " <> show e)
    Right addr ->
      pure
        ( addr,
          toStrict $ TB.toLazyText (render Short addr),
          toStrict $ TB.toLazyText (render Full addr)
        )

-- | Test that parsing the rendered output and then rendering it again
-- produces the same results.
idempotentTest :: Text -> Text -> Assertion
idempotentTest simple full = do
  (_, x, y) <- roundTrip Parser.Strict simple
  x @?= simple -- Parsing and rending a simple address should
  y @?= simple -- produce a simple rendering both times.
  (_, z, w) <- roundTrip Parser.Strict full
  z @?= simple
  w @?= full
