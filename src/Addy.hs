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
module Addy
  ( -- * How to use this library
    -- $use

    -- * Decoding and encoding
    decode,
    decodeLenient,
    encode,
    encodeFull,

    -- * Email addresses
    EmailAddr,
    emailAddr,
    displayName,
    localPart,
    domain,
    comments,

    -- * Display name
    DisplayName,
    _DisplayName,
    validateDisplayName,

    -- * Local part
    LocalPart,
    _LocalPart,
    validateLocalPart,

    -- * Domain part
    Domain (..),
    _Domain,
    _DomainLiteral,
    DomainName,
    _DomainName,
    validateDomainName,

    -- * Host names
    HostName,
    _HostNames,
    _HostName,
    validateHostName,

    -- * Address literals
    AddressLiteral (..),
    _IpAddressLiteral,
    _TaggedAddressLiteral,
    _AddressLiteral,
    AddressTag,
    _AddressTag,
    validateAddressTag,
    Literal,
    _Literal,
    validateLiteral,

    -- * Comments
    Comment (..),
    _Comment,
    commentLoc,
    commentContent,
    CommentLoc (..),
    CommentContent,
    _CommentContent,
    validateCommentContent,
  )
where

import Addy.Internal.Parser as P
import Addy.Internal.Render as R
import Addy.Internal.Types
import Addy.Internal.Validation
import Control.Lens (Prism', prism')
import Validation.Combinators (successToMaybe)

-- | Decode an email address.
--
-- @since 0.1.0.0
decode :: Text -> Either (NonEmpty Error) EmailAddr
decode = P.parseWithMode P.Strict

-- | Decode an email address, allowing obsolete characters.  The
-- obsolete characters are parsed but not included in the output.
--
-- @since 0.1.0.0
decodeLenient :: Text -> Either (NonEmpty Error) EmailAddr
decodeLenient = P.parseWithMode P.Lenient

-- | Encode an email address as text.  This function produces the
-- short form of an email address.  That is, just the 'LocalPart' and
-- the 'Domain' separated by @\@@.
--
-- @since 0.1.0.0
encode :: EmailAddr -> Text
encode = R.renderToText R.Short

-- | Encode a complete email address to text, including the optional
-- display name and any comments.
--
-- @since 0.1.0.0
encodeFull :: EmailAddr -> Text
encodeFull = R.renderToText R.Full

-- | Build an 'EmailAddr' from a 'LocalPart' and 'Domain'.
--
-- @since 0.1.0.0
emailAddr :: LocalPart -> Domain -> EmailAddr
emailAddr l d = EmailAddr Nothing l d []

-- | Prism for working with display names.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text into a 'DisplayName' with content validation:
--
-- >>> isJust ("Some Text" ^? _DisplayName)
-- True
-- >>> "Some\nText" ^? _DisplayName
-- Nothing -- Validation failed.
--
-- To access the text content of a 'DisplayName':
--
-- >>> review _DisplayName someDisplayName
-- "Some Text"
--
-- Uses 'validateDisplayName' to perform validation.
--
-- @since 0.1.0.0
_DisplayName :: Prism' Text DisplayName
_DisplayName =
  prism'
    displayNameText
    (validateDisplayName >>> successToMaybe)

-- | Prism for working with the local part of an email address.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to a 'LocalPart' with content validation:
--
-- >>> isJust ("cockroach+mouse" ^? _LocalPart)
-- True
-- >>> "cockroach\nmouse" ^? _LocalPart
-- Nothing -- Validation failed.
--
-- To access the text content of a 'LocalPart':
--
-- >>> review _LocalPart someLocalPart
-- "cockamouse"
--
-- Uses 'validateLocalPart' to perform validation.
--
-- @since 0.1.0.0
_LocalPart :: Prism' Text LocalPart
_LocalPart =
  prism'
    localPartText
    (validateLocalPart >>> successToMaybe)

-- | Prism for working with domain names.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to a 'DomainName' with validation:
--
-- >>> isJust ("gmail.com" ^? _DomainName)
-- True
-- >>> "too.many.dots." ^? _DomainName
-- Nothing
--
-- To access the text content of a 'DomainName':
--
-- >>> review _DomainName someDomainName
-- "gmail.com"
--
-- Uses 'validateDomainName' to perform validation.
--
-- @since 0.1.0.0
_DomainName :: Prism' Text DomainName
_DomainName =
  prism'
    domainNameText
    (validateDomainName >>> successToMaybe)

-- | Prism for working with host names (DNS labels).
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to a host name with validation:
--
-- >>> isJust ("com" ^? _HostName)
-- True
-- >>> "com." ^? _HostName
-- Nothing -- Validation failed.
--
-- To access the text content of a 'HostName':
--
-- >>> review _HostName someHostName
-- "com"
--
-- Uses 'validateHostName' to perform validation.
--
-- @since 0.1.0.0
_HostName :: Prism' Text HostName
_HostName =
  prism'
    hostNameText
    (validateHostName >>> successToMaybe)

-- | Prism for working with the 'AddressTag' for an 'AddressLiteral'.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to an address tag with validation:
--
-- >>> isJust ("IPv6" ^? _AddressTag)
-- True
-- >>> "[IPv6]" ^? _AddressTag
-- Nothing -- Validation failed.
--
-- To access the text content of an 'AddressTag':
--
-- >>> review _AddressTag someTag
-- "tag"
--
-- Uses 'validateAddressTag' to perform validation.
--
-- @since 0.1.0.0
_AddressTag :: Prism' Text AddressTag
_AddressTag =
  prism'
    addressTagText
    (validateAddressTag >>> successToMaybe)

-- | Prism for working with the literal text of an address literal.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to an address literal with validation:
--
-- >>> isJust ("127.0.0.1" ^? _Literal)
-- True
-- >>> "[]" ^? _Literal
-- Nothing -- Validation failed.
--
-- To access the text content of a 'Literal':
--
-- >>> review _Literal someLiteral
-- "127.0.0.1"
--
-- Uses 'validateLiteral' to perform validation.
--
-- @since 0.1.0.0
_Literal :: Prism' Text Literal
_Literal =
  prism'
    literalText
    (validateLiteral >>> successToMaybe)

-- | Prism for working with the text content of a comment.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text to a comment with validation:
--
-- >>> isJust ("best email" ^? _CommentContent)
-- True
-- >>> "\n" ^? _CommentContent
-- Nothing
--
-- To access the text content of the comment:
--
-- >>> review _CommentContent someComment
-- "super"
--
-- Uses 'validateCommentContent' to perform validation.
--
-- @since 0.1.0.0
_CommentContent ::
  Prism' Text CommentContent
_CommentContent =
  prism'
    commentContentText
    (validateCommentContent >>> successToMaybe)

-- $use
--
-- This library is designed to be imported qualified:
--
-- > import qualified Addy
--
-- To decode an email address from text:
--
-- >>> Addy.decode "example@example.com"
-- Right (EmailAddr {
--   displayName = Nothing,
--   localPart = "example",
--   domain = Domain "example.com",
--   comments = []})
--
-- >>> Addy.decode "我買@屋企.香港"
-- Right (EmailAddr {
--   displayName = Nothing,
--   localPart = "\25105\36023",
--   domain = Domain "\23627\20225.\39321\28207",
--   comments = []})
--
-- >>> Addy.decode "Mary Smith <mary@example.net> (hi there!)"
-- Right (EmailAddr {
--   displayName = Just "Mary Smith",
--   localPart = "mary",
--   domain = Domain "example.net",
--   comments = [Comment AfterAddress "hi there!"]})
--
-- >>> Addy.decode "example@[127.0.0.1]"
-- Right (EmailAddr {
--   displayName = Nothing,
--   localPart = "example",
--   domain = DomainLiteral (IpAddressLiteral (ipv4 127 0 0 1)),
--   comments = []})
--
-- Turning an email address back to text is just as easy:
--
-- >>> Addy.encode address
-- > "example@example.com"
--
-- >>> :{
--   Addy.decode "Mary Smith <mary@example.net> (hi there!)"
--     & second Addy.encodeFull
-- :}
-- Right "Mary Smith <mary@example.net> (hi there!)"
--
-- Lens and prisms are provided to make working with an email address easier:
--
-- > import qualified Addy
-- > import Control.Lens
-- >
--
-- >>> :{
--  Addy.emailAddr
--    <$> "pjones" ^? Addy_LocalPart
--    <*> "devalot.com" ^? Addy._DomainName.re Addy_Domain
-- :}
-- > Just (EmailAddr {
-- >   displayName = Nothing,
-- >   localPart = "pjones",
-- >   domain = Domain "devalot.com",
-- >   comments = []})
-- >
-- > >>> Addy.decode "example@example.com" ^? _Right.Addy.domain
-- > Just (Domain "example.com")
--
-- Or you can just use the validation functions directly:
--
-- >>> :{
--  Addy.emailAddr
--    <$> Addy.validateLocalPart "pjones"
--    <*> (Addy.Domain <$> Addy.validateDomainName "devalot.com")
-- :}
-- Success (EmailAddr {
--   displayName = Nothing,
--   localPart = "pjones",
--   domain = Domain "devalot.com",
--   comments = []})
