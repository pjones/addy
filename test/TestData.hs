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
module TestData
  ( IsEmailTest (..),
    IsEmailCat (..),
    isEmailTests,
    rfc5322Examples,
    rfc5322ObsExamples,
    wikipediaIntExamples,
    miscExamples,
    genShortEmail,
    genLongEmail,
  )
where

import Addy.Internal.Parser (utf8NonAscii)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Char
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Types.URI (urlDecode)
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
  deriving (Eq, Show)

isEmailTests :: IO [(IsEmailTest, IsEmailCat)]
isEmailTests = do
  let fileName = "test/isemail.json"
  tests <- (Aeson.decode <$> readFileLBS fileName) >>= \case
    Nothing -> assertFailure "failed to load the isemail.json file"
    Just ts -> pure ts
  pure $ map (\t -> (decodeAddr t, decodeCat t)) tests

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

rfc5322Examples :: [(Text, Text)]
rfc5322Examples =
  [ ("John Doe <jdoe@machine.example>", "jdoe@machine.example"),
    ("Mary Smith <mary@example.net>", "mary@example.net"),
    ("Michael Jones <mjones@machine.example>", "mjones@machine.example"),
    ("\"Joe Q. Public\" <john.q.public@example.com>", "john.q.public@example.com"),
    ("Mary Smith <mary@x.test>", "mary@x.test"),
    ("jdoe@example.org", "jdoe@example.org"),
    ("Who? <one@y.test>", "one@y.test"),
    ("<boss@nil.test>", "boss@nil.test"),
    ("\"Giant; \\\"Big\\\" Box\" <sysservices@example.net>", "sysservices@example.net"),
    ("Pete <pete@silly.example>", "pete@silly.example"),
    ("Ed Jones <c@a.test>", "c@a.test"),
    ("joe@where.test", "joe@where.test"),
    ("John <jdoe@one.test>", "jdoe@one.test"),
    ("\"Mary Smith: Personal Account\" <smith@home.example>", "smith@home.example"),
    ("Jane Brown <j-brown@other.example>", "j-brown@other.example"),
    ("John <jdoe@one.test> (my dear friend)", "jdoe@one.test")
  ]

rfc5322ObsExamples :: [(Text, Text, Text)]
rfc5322ObsExamples =
  [ ( "Pete(A nice \\) chap) <pete(his account)@silly.test(his host)>",
      "pete@silly.test",
      "Pete(A nice \\) chap) <pete@silly.test(his host)>"
    ),
    ( "Chris Jones <c@(Chris's host.)public.example>",
      "c@public.example",
      "Chris Jones <c@public.example>"
    )
  ]

wikipediaIntExamples :: [Text]
wikipediaIntExamples =
  [ "Pelé@example.com",
    "δοκιμή@παράδειγμα.δοκιμή",
    "我買@屋企.香港",
    "二ノ宮@黒川.日本",
    "медведь@с-балалайкой.рф",
    "संपर्क@डाटामेल.भारत"
  ]

-- | Other examples that should pass.
--
-- > (source, simple-format)
miscExamples :: [(Text, Text)]
miscExamples =
  [ ("example+label@example.com", "example+label@example.com"),
    ("a@b", "a@b")
  ]

genLocalPart :: Gen Text
genLocalPart =
  Gen.filter okay (Gen.text (Range.linear 1 64) unicode)
  where
    okay :: Text -> Bool
    okay t =
      Text.all allowedChar t
        && not (Text.isPrefixOf "." t)
        && not (Text.isSuffixOf "." t)

genDomain :: Gen Text
genDomain =
  Gen.filter okay (Gen.text (Range.linear 1 254) unicode)
  where
    okay :: Text -> Bool
    okay t =
      Text.all allowedChar t
        && not (Text.isPrefixOf "-" t)
        && not (Text.isSuffixOf "-" t)

genShortEmail :: Gen Text
genShortEmail = do
  localPart <- genLocalPart
  domain <- genDomain
  pure (localPart <> "@" <> domain)

genLongEmail :: Gen Text
genLongEmail = do
  display <- Gen.text (Range.linear 1 25) unicode
  localPart <- genLocalPart
  domain <- genDomain
  pure $
    mconcat
      [ display,
        " <",
        localPart,
        "@",
        domain,
        ">"
      ]

allowedChar :: Char -> Bool
allowedChar c = isAscii c || utf8NonAscii c

-- | Modified 'unicode' generator from Hedgehog.
unicode :: Gen Char
unicode =
  Gen.frequency
    [ (55296, Gen.element "!#$%&'*+-/=?^_`{|}~"),
      (55296, Gen.alphaNum),
      (8190, Gen.filter isPrint (Gen.enum (chr 0xc2) maxBound))
    ]
