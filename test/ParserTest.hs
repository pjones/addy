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
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Network.HTTP.Types.URI (urlDecode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

data IsEmailTest = IET
  { ietId :: Int,
    ietAddr :: Text,
    ietCat :: Text,
    ietDia :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data IsEmailCat
  = CatError
  | CatDeprec
  | CatOkay
  deriving (Show)

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
  mapM_
    go
    [ "John Doe <jdoe@machine.example>",
      "Mary Smith <mary@example.net>",
      "Michael Jones <mjones@machine.example>",
      "\"Joe Q. Public\" <john.q.public@example.com>",
      "Mary Smith <mary@x.test>",
      "jdoe@example.org",
      "Who? <one@y.test>",
      "<boss@nil.test>",
      "\"Giant; \\\"Big\\\" Box\" <sysservices@example.net>",
      "Pete <pete@silly.example>",
      "Ed Jones <c@a.test>",
      "joe@where.test",
      "John <jdoe@one.test>",
      "\"Mary Smith: Personal Account\" <smith@home.example>",
      "Jane Brown <j-brown@other.example>",
      "John <jdoe@one.test> (my dear friend)"
    ]
  -- Only works in lenient mode:
  assertParse Lenient "Pete(A nice \\) chap) <pete(his account)@silly.test(his host)>"
  assertParse Lenient "Chris Jones <c@(Chris's host.)public.example>"
  where
    go :: Text -> Assertion
    go t = do
      assertParse Strict t
      assertParse Lenient t

testWikipediaIntExamples :: Assertion
testWikipediaIntExamples =
  mapM_
    go
    [ "Pelé@example.com",
      "δοκιμή@παράδειγμα.δοκιμή",
      "我買@屋企.香港",
      "二ノ宮@黒川.日本",
      "медведь@с-балалайкой.рф",
      "संपर्क@डाटामेल.भारत"
    ]
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
testParserWithIsEmail = do
  let fileName = "test/isemail.json"
  tests <- (Aeson.decode <$> readFileLBS fileName) >>= \case
    Nothing -> assertFailure "failed to load the isemail.json file"
    Just ts -> pure ts
  mapM_ (decodeAddr >>> go) (tests :: [IsEmailTest])
  where
    go :: IsEmailTest -> Assertion
    go isemail = case decodeCat isemail of
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

-- | The XSL file URL-encodes the email addresses.  Additionally, the
-- XML file encodes some characters by adding 0x2400 to them.
decodeAddr :: IsEmailTest -> IsEmailTest
decodeAddr iet@IET {ietAddr} =
  iet
    { ietAddr = unCtrl (decode ietAddr)
    }
  where
    decode :: Text -> Text
    decode = decodeUtf8 . urlDecode False . encodeUtf8
    unCtrl :: Text -> Text
    unCtrl =
      let f c =
            if c >= '\x2400' && c <= '\x241f'
              then chr (ord c - 0x2400)
              else c
       in Text.map f

decodeCat :: IsEmailTest -> IsEmailCat
decodeCat iet = go & override
  where
    go = case ietCat iet of
      "ISEMAIL_ERR" -> CatError
      "ISEMAIL_DEPREC" -> CatDeprec
      _ -> CatOkay
    override cat
      -- This one is a bit silly.  Even if that form is deprecated
      -- it would then pass under the general @domain-literal@ rule
      -- in RFC 5322 §3.4.1.
      | ietId iet == 71 = CatOkay
      -- RFC 5322 (and its errata) clearly allows whitespace before
      -- and after the at sign.
      | ietId iet == 85 = CatOkay
      -- It's not worth supporting the very obsolete syntax of having
      -- multiple empty lines before an email address.
      | ietId iet == 89 = CatError
      | ietId iet == 149 = CatError
      -- These should be marked at deprecated.
      | ietId iet == 115 = CatDeprec
      | ietId iet == 116 = CatDeprec
      | ietId iet == 117 = CatDeprec
      -- Nothing to override.
      | otherwise = cat
