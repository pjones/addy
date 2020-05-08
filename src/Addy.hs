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
-- Email addressed are complicated, really complicated.  This library
-- supports all standardized forms of email addresses, including those
-- with UTF-8 encoded Unicode characters.  The standards used by this
-- library include:
--
--   * RFC 1123: Requirements for Internet Hosts -- Application and Support
--   * RFC 2181: Clarifications to the DNS Specification
--   * RFC 3696: Application Techniques for Checking and Transformation of Names
--   * RFC 5321: Simple Mail Transfer Protocol
--   * RFC 5322: Internet Message Format
--   * RFC 6531: SMTP Extension for Internationalized Email
--   * RFC 6532: Internationalized Email Headers
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
    emailAddrLit,
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
-- This is useful for exacting email addresses from mail messages but
-- should not be used to validate user input.
--
-- >>> Addy.decode "my . email . addy@(WTF)example.com"
-- Left (ParserFailedError "local part > quoted content > '\"': Failed reading: satisfy" :| [])
--
-- >>> Addy.decodeLenient "my . email . addy@(WTF)example.com"
-- Right (EmailAddr "my.email.addy@example.com (WTF)")
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

-- | Build an 'EmailAddr' from a 'LocalPart' and 'DomainName'.
--
-- @since 0.1.0.0
emailAddr :: LocalPart -> DomainName -> EmailAddr
emailAddr l d = EmailAddr Nothing l (Domain d) []

-- | Build an 'EmailAddr' from a 'LocalPart' and an 'AddressLiteral'.
--
-- @since 0.1.0.0
emailAddrLit :: LocalPart -> AddressLiteral -> EmailAddr
emailAddrLit l d = EmailAddr Nothing l (DomainLiteral d) []

-- | Prism for working with display names.
--
-- > import Control.Lens ((^?), review)
--
-- To convert text into a 'DisplayName' with content validation:
--
-- >>> "Some Text" ^? _DisplayName
-- Just (DisplayName "Some Text")
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
-- >>> "cockroach+mouse" ^? _LocalPart
-- Just (LocalPart "cockroach+mouse")
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
-- >>> "gmail.com" ^? _DomainName
-- Just (DomainName "gmail.com")
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
-- >>> "com" ^? _HostName
-- Just (HostName "com")
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
-- >>> "IPv6" ^? _AddressTag
-- Just (AddressTag "IPv6")
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
-- >>> "127.0.0.1" ^? _Literal
-- Just (Literal "127.0.0.1")
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
-- >>> "best email ever" ^? _CommentContent
-- Just (CommentContent "best email ever")
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
-- == Importing
--
-- This library is designed to be imported qualified:
--
-- > import qualified Addy
--
-- == Decoding addresses
--
-- To decode (parse) an email address from text:
--
-- >>> Addy.decode "example@example.com"
-- Right (EmailAddr "example@example.com")
--
-- >>> Addy.decode "我買@屋企.香港"
-- Right (EmailAddr "\25105\36023@\23627\20225.\39321\28207")
--
-- >>> Addy.decode "Mary Smith <mary@example.net> (hi there!)"
-- Right (EmailAddr "Mary Smith <mary@example.net> (hi there!)")
--
-- >>> Addy.decode "example@[127.0.0.1]"
-- Right (EmailAddr "example@[127.0.0.1]")
--
-- After decoding, an address is automatically normalized by performing
-- NFC normalization and down-casing the domain name:
--
-- >>> Addy.decode "My.Email.Addy@ExAmPlE.COM"
-- Right (EmailAddr "My.Email.Addy@example.com")
--
-- == Encoding addresses
--
-- Turning an email address back to text is just as easy:
--
-- >>> Addy.encode address
-- "example@example.com"
--
-- If an address has an optional display name or comments you can
-- render those with the 'encodeFull' function.
--
-- >>> :{
--   Addy.decode "Mary Smith <mary@example.net> (hi there!)"
--     & second Addy.encodeFull
-- :}
-- Right "Mary Smith <mary@example.net> (hi there!)"
--
-- == Creating addresses
--
-- In order to prevent invalid email addresses from being created this
-- library uses @newtype@ wrappers and does not export the data
-- constructors.  Therefore you'll need to use the smart constructor
-- approach using the 'emailAddr' function.
--
-- If you want to work with the validation functions directly we
-- recommend 'Applicative' syntax:
--
-- >>> :{
--  Addy.emailAddr
--    <$> Addy.validateLocalPart "pjones"
--    <*> Addy.validateDomainName "devalot.com"
-- :}
-- Success (EmailAddr "pjones@devalot.com")
--
-- Prisms for the @newtype@ wrappers are provided if you want to use optics:
--
-- >>> :{
--  Addy.emailAddr
--    <$> "pjones" ^? Addy._LocalPart
--    <*> "devalot.com" ^? Addy._DomainName
-- :}
-- Just (EmailAddr "pjones@devalot.com")
--
-- == Optics
--
-- Lens and prisms are provided to make working with email addresses easier:
--
-- > import qualified Addy
-- > import Control.Lens
--
-- >>> Addy.decode "example@example.com" ^? _Right . Addy.domain
-- Just (Domain (DomainName "example.com"))
--
-- >>> Addy.decode "example@example.com"
--   ^? _Right . Addy.domain . Addy._Domain . Addy._HostNames
-- Just [HostName "example",HostName "com"]
--
-- >>> Addy.decode "example@example.com"
--   ^.. _Right . Addy.domain . Addy._Domain
--   . Addy._HostNames . traversed . re _HostName
-- ["example","com"]
--
-- == A word about address literals
--
-- Believe it or not, this is a completely valid email address:
--
-- >>> Addy.decode "example@[what's my domain name?]"
--   ^? _Right . Addy.domain
-- Just (DomainLiteral (AddressLiteral (Literal "what's my domain name?")))
--
-- If you're working with email messages it might be useful to capture
-- these address literals, especially if you know how to interpret
-- them.  However, if you're validating user input you probably don't
-- want to allow these.
--
-- >>> Addy.decode "e@[127.0.0.1]" ^? _Right
--   >>= failover (Addy.domain . Addy._Domain) id
-- Nothing
--
-- >>> Addy.decode "example@example.com" ^? _Right
--   >>= failover (Addy.domain . Addy._Domain) id
-- Just (EmailAddr "example@example.com")
