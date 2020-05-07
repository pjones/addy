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
--
-- Internal data validation functions.
module Addy.Internal.Validation
  ( validateHostName,
    validateDomainName,
    validateLocalPart,
    validateDisplayName,
    validateLiteral,
    validateAddressTag,
    validateCommentContent,
    validateEmailAddr,
  )
where

import Addy.Internal.Char
import Addy.Internal.Types
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.ICU as ICU
import Validation

-- | Validate a single host name.  Each host name in a domain name
-- (referred to as a /label/) must validate with this function.
--
-- RFC 2181 §11 clearly states that there are no restrictions placed
-- on which characters may appear in a label.  However, due to legacy
-- issues we enforce the rule from RFC 952 §1 that disallows hyphens
-- as the first or last character of a label.
--
-- RFC 5322 §3.4.1 restricts the characters that may appear in the
-- domain component of an /email address/.  Even though a DNS label
-- does not impose such restrictions, in order to be a valid email
-- address the label must only be composed of so-called @atext@
-- characters or @UTF8-non-ascii@ characters.
--
-- Finally, RFC 2181 §11 restricts the length of a label to 63 bytes
-- and the fully-qualified domain name to 255 bytes.  RFC 6532 which
-- extends the email syntax to allow UTF-8 encoded Unicode characters
-- briefly states in §3.4 to continue using bytes, and not
-- characters.  It also states that Unicode text should be normalized
-- (which we do).
--
-- @since 0.1.0.0
validateHostName :: Text -> Validation (NonEmpty Error) HostName
validateHostName content =
  let content' = Text.toLower (ICU.normalize ICU.NFC content)
   in HN content'
        <$ ( validateNotPrefix "-" content'
               *> validateNotSuffix "-" content'
               *> validateAllowedChars atext content'
               *> validateLength 1 63 content'
           )

-- | Validate a domain name.
--
-- The domain name is split into host names (labels) and each label is
-- validated with 'validateHostName'.
--
-- @since 0.1.0.0
validateDomainName :: Text -> Validation (NonEmpty Error) DomainName
validateDomainName name =
  fromHostList <$> (validateLength 1 255 name *> validHostList)
  where
    validHostList :: Validation (NonEmpty Error) [HostName]
    validHostList =
      foldr
        ( \h hs -> (:) <$> validateHostName h <*> hs
        )
        (pure [])
        (Text.splitOn "." name)
    fromHostList :: [HostName] -> DomainName
    fromHostList hs =
      map coerce hs
        & Text.intercalate "."
        & DN

-- | Validate and normalize the text content of the 'LocalPart' of an
-- email address.
--
-- RFC 3696 §3 restricts the length of the local part to a maximum of
-- 64 bytes.  RFC 6532 extends the character set to include Unicode
-- characters but maintains the length measurement as bytes and not
-- characters.
--
-- @since 0.1.0.0
validateLocalPart ::
  Text -> Validation (NonEmpty Error) LocalPart
validateLocalPart content =
  let content' = ICU.normalize ICU.NFC content
   in LP content'
        <$ ( validateLength 1 64 content'
               *> validateAllowedChars allowedChar content'
           )
  where
    allowedChar :: Char -> Bool
    allowedChar c = atext c || c == '.' || qtext c || quotedPair c

-- | Validate the content of a 'DisplayName'.
--
-- There does not appear to be a limit on the length of the display
-- name.  For consistency and efficiency we limit it to 64 bytes, the
-- same as the local part.
--
-- @since 0.1.0.0
validateDisplayName :: Text -> Validation (NonEmpty Error) DisplayName
validateDisplayName content =
  DP content
    <$ ( validateLength 1 64 content
           *> validateAllowedChars allowedChar content
       )
  where
    allowedChar :: Char -> Bool
    allowedChar c = atext c || qtext c || quotedPair c

-- | Validate the 'Literal' content of a domain literal.
--
-- There does not appear to be a limit on the length of an address
-- literal but for consistency with DNS labels we limit them to 63
-- bytes.
--
-- @since 0.1.0.0
validateLiteral :: Text -> Validation (NonEmpty Error) Literal
validateLiteral content =
  Lit content
    <$ ( validateLength 1 63 content
           *> validateAllowedChars allowedChar content
       )
  where
    allowedChar :: Char -> Bool
    allowedChar c = dtext c || wsp c || c == '\r' || c == '\n'

-- | Validate the content of an 'AddressTag'.  Uses the same rules as
-- 'validateLiteral'.
--
-- @since 0.1.0.0
validateAddressTag :: Text -> Validation (NonEmpty Error) AddressTag
validateAddressTag content = AT content <$ validateLiteral content

-- | Validate the content of a comment.
--
-- There does not appear to be a limit on the length of a comment.
-- For consistency and efficiency we limit it to 64 bytes, the same as
-- the local part.
--
-- @since 0.1.0.0
validateCommentContent :: Text -> Validation (NonEmpty Error) CommentContent
validateCommentContent content =
  CC content
    <$ ( validateLength 1 64 content
           *> validateAllowedChars allowedChar content
       )
  where
    allowedChar :: Char -> Bool
    allowedChar c = ctext c || quotedPair c

-- | Validate an entire 'EmailAddr'.  This is used by the parser to
-- validate rules that are not encoded in the various component parsers.
--
-- @since 0.1.0.0
validateEmailAddr :: EmailAddr -> Validation (NonEmpty Error) EmailAddr
validateEmailAddr EmailAddr {..} =
  EmailAddr
    <$> displayNameV
    <*> validateLocalPart (localPartText _localPart)
    <*> domainV
    <*> commentsV
  where
    displayNameV :: Validation (NonEmpty Error) (Maybe DisplayName)
    displayNameV = case _displayName of
      Nothing -> pure Nothing
      Just (DP t) -> Just <$> validateDisplayName t
    domainV :: Validation (NonEmpty Error) Domain
    domainV = case _domain of
      Domain (DN t) -> Domain <$> validateDomainName t
      DomainLiteral lit -> DomainLiteral <$> addrLiteralV lit
    addrLiteralV :: AddressLiteral -> Validation (NonEmpty Error) AddressLiteral
    addrLiteralV = \case
      IpAddressLiteral ip ->
        pure (IpAddressLiteral ip)
      TaggedAddressLiteral (AT at) (Lit lit) ->
        TaggedAddressLiteral <$> validateAddressTag at <*> validateLiteral lit
      AddressLiteral (Lit t) ->
        AddressLiteral <$> validateLiteral t
    commentsV :: Validation (NonEmpty Error) [Comment]
    commentsV =
      foldr
        ( \(Comment loc (CC t)) cs ->
            (:) . Comment loc <$> validateCommentContent t <*> cs
        )
        (pure [])
        _comments

-- |  Validate that the given text does not begin with the given prefix.
--
-- @since 0.1.0.0
validateNotPrefix :: Text -> Text -> Validation (NonEmpty Error) ()
validateNotPrefix prefix name =
  failureIf (Text.isPrefixOf prefix name) (InvalidPrefixError prefix)

-- | Validate that the given text does not end with the given suffix.
--
-- @since 0.1.0.0
validateNotSuffix :: Text -> Text -> Validation (NonEmpty Error) ()
validateNotSuffix suffix name =
  failureIf (Text.isSuffixOf suffix name) (InvalidSuffixError suffix)

-- | Validate that the text only contains characters for which the
-- given function returns true.
--
-- @since 0.1.0.0
validateAllowedChars :: (Char -> Bool) -> Text -> Validation (NonEmpty Error) ()
validateAllowedChars f t =
  failureUnless (Text.all f t) (InvalidCharactersError $ Text.filter (not . f) t)

-- | Validate the length of the given text falls within the given
-- @min@ and @max@ values.
--
-- @since 0.1.0.0
validateLength :: Int -> Int -> Text -> Validation (NonEmpty Error) ()
validateLength minL maxL t =
  let bytes = ByteString.length (encodeUtf8 t)
   in failureIf
        (bytes < minL || bytes > maxL)
        (InvalidLengthError minL maxL bytes)
