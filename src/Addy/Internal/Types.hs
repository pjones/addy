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
-- The module exports internal types along with their constructors.
--
-- The rendering code relies on the newtype wrappers around 'Text' to
-- keep out invalid characters.  Prefer to use the official interface
-- if possible.
module Addy.Internal.Types
  ( Error (..),
    EmailAddr (..),
    displayName,
    localPart,
    domain,
    comments,
    DisplayName (..),
    LocalPart (..),
    Domain (..),
    _Domain,
    _DomainLiteral,
    DomainName (..),
    HostName (..),
    _HostNames,
    AddressLiteral (..),
    _IpAddressLiteral,
    _TaggedAddressLiteral,
    _AddressLiteral,
    AddressTag (..),
    Literal (..),
    Comment (..),
    _Comment,
    commentLoc,
    commentContent,
    CommentLoc (..),
    CommentContent (..),
  )
where

import Control.Lens (Iso', Lens', Prism', iso, lens, prism')
import qualified Data.Text as Text
import Net.IP (IP)

-- | Potential validation errors.
--
-- @since 0.1.0.0
data Error
  = -- | A component of an email address may not start with the
    -- recorded prefix text.
    InvalidPrefixError Text
  | -- | A component of an email address may not end with the recorded
    -- suffix text.
    InvalidSuffixError Text
  | -- | A component of an email address contains invalid characters.
    InvalidCharactersError Text
  | -- | A component of an email address does not meet the set length
    -- requirements.  The values in this constructor are @min@, @max@,
    -- and @actual@.
    InvalidLengthError Int Int Int
  | -- | The input to the address decoder was not a valid email
    -- address and produced the recorded error message.
    ParserFailedError Text
  deriving (Show, Eq)

-- | The representation of a complete email address.
--
-- The parser preserves optional components such as the display name
-- and comments.  The rendering code can optionally include these
-- optional elements when turning the address back into 'Text'.
--
-- @since 0.1.0.0
data EmailAddr = EmailAddr
  { _displayName :: Maybe DisplayName,
    _localPart :: LocalPart,
    _domain :: Domain,
    _comments :: [Comment]
  }
  deriving (Show)

-- | Optional display name.  Addresses in the @name-addr@ format
-- from RFC 5322 allow descriptive text to precede the address.
-- This is commonly used in email messages to list the name of the
-- address' owner.
--
-- @since 0.1.0.0
displayName :: Lens' EmailAddr (Maybe DisplayName)
displayName = lens _displayName (\e d -> e {_displayName = d})

-- | The 'LocalPart' of an email address usually references the
-- destination mailbox on the 'Domain' server.  However, the
-- content of the 'LocalPart' can only be understood by the
-- receiving 'Domain'.
--
-- @since 0.1.0.0
localPart :: Lens' EmailAddr LocalPart
localPart = lens _localPart (\e l -> e {_localPart = l})

-- | The 'Domain' refers to the fully-qualified domain name that
-- accepts mail for the associated 'LocalPart'.  See the
-- documentation for the 'Domain' type for more details.
--
-- @since 0.1.0.0
domain :: Lens' EmailAddr Domain
domain = lens _domain (\e d -> e {_domain = d})

-- | Addresses in both the @name-addr@ and @addr-spec@ formats
-- support comments.
--
-- @since 0.1.0.0
comments :: Lens' EmailAddr [Comment]
comments = lens _comments (\e cs -> e {_comments = cs})

-- | Optional display name.  Usually this is the name of the person
-- who receives email at the associated address.
--
-- > Display Name <example@example.com>
--
-- @since 0.1.0.0
newtype DisplayName = DP
  { displayNameText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | The name of the mailbox on the associated 'Domain'.
--
-- @since 0.1.0.0
newtype LocalPart = LP
  { localPartText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | A fully-qualified domain name /or/ an address literal.
--
-- Most email addresses use a domain name.  However, it's perfectly
-- legal to use an 'AddressLiteral' instead.
--
-- @since 0.1.0.0
data Domain
  = Domain DomainName
  | DomainLiteral AddressLiteral
  deriving (Show, Eq)

-- | Prism for working with domain names.
--
-- @since 0.1.0.0
_Domain :: Prism' Domain DomainName
_Domain =
  prism'
    Domain
    ( \case
        Domain dn -> Just dn
        DomainLiteral {} -> Nothing
    )

-- | Prism for working with domain literals.
--
-- @since 0.1.0.0
_DomainLiteral :: Prism' Domain AddressLiteral
_DomainLiteral =
  prism'
    DomainLiteral
    ( \case
        Domain {} -> Nothing
        DomainLiteral dl -> Just dl
    )

-- | A fully-qualified domain name which is made up of a list of
-- host names (labels) separated by dots.
--
-- @since 0.1.0.0
newtype DomainName = DN
  { domainNameText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | The name of one host component of a domain name.
--
-- @since 0.1.0.0
newtype HostName = HN
  { hostNameText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | Iso for converting between domain names and a list of host names.
--
-- >>> "gmail.uk.co" ^. _DomainName._HostNames & map (review _HostName)
-- ["gmail","uk","co"]
--
-- @since 0.1.0.0
_HostNames :: Iso' DomainName [HostName]
_HostNames =
  iso
    (domainNameText >>> Text.splitOn "." >>> map HN)
    (map hostNameText >>> Text.intercalate "." >>> DN)

-- | Address literals can be used instead of a domain name to direct
-- mail to a specific IP address or other tagged address type.
--
-- Example email addresses with address literals:
--
-- > example@[127.0.0.1]
-- > example@[IPv6:1111:2222:3333:4444:5555:6666:7777]
-- > example@[Just-some-text]
--
-- @since 0.1.0.0
data AddressLiteral
  = -- | A literal IP address as defined in RFC 5321 §4.1.3.  The
    -- address can be in many formats so it is presented here in its
    -- parsed form.
    IpAddressLiteral IP
  | -- | RFC 5321 also defines a /general address literal/ where a
    -- /standardized tag/ precedes the address itself.  The only
    -- information provided about the standardized tag is:
    --
    -- > Standardized-tag MUST be specified in a
    -- > Standards-Track RFC and registered with IANA
    TaggedAddressLiteral AddressTag Literal
  | -- | RFC 5322 defines a @domain-literal@ as (roughly) a span of
    -- characters that are allowed in a domain name.  The
    -- interpretation of those characters is left to \"separate
    -- documents\" such as RFC 5321.
    --
    -- If an address literal cannot be parsed in one of the proceeding
    -- formats it is encoded as a 'Literal' value.
    AddressLiteral Literal
  deriving (Show, Eq)

-- | Prism for working with IP address literals.
--
-- @since 0.1.0.0
_IpAddressLiteral :: Prism' AddressLiteral IP
_IpAddressLiteral =
  prism'
    IpAddressLiteral
    ( \case
        IpAddressLiteral ip -> Just ip
        TaggedAddressLiteral {} -> Nothing
        AddressLiteral {} -> Nothing
    )

-- | Prism for working with tagged address literals.
--
-- @since 0.1.0.0
_TaggedAddressLiteral :: Prism' AddressLiteral (AddressTag, Literal)
_TaggedAddressLiteral =
  prism'
    (uncurry TaggedAddressLiteral)
    ( \case
        IpAddressLiteral {} -> Nothing
        TaggedAddressLiteral tag body -> Just (tag, body)
        AddressLiteral {} -> Nothing
    )

-- | Prism for working with address literals.
--
-- @since 0.1.0.0
_AddressLiteral :: Prism' AddressLiteral Literal
_AddressLiteral =
  prism'
    AddressLiteral
    ( \case
        IpAddressLiteral {} -> Nothing
        TaggedAddressLiteral {} -> Nothing
        AddressLiteral lit -> Just lit
    )

-- | A tag that can be used with a 'TaggedAddressLiteral'.
--
-- @since 0.1.0.0
newtype AddressTag = AT
  { addressTagText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | A literal address that can be used with a 'TaggedAddressLiteral'
-- or 'AddressLiteral'.
--
-- @since 0.1.0.0
newtype Literal = Lit
  { literalText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)

-- | A comment which may appear in an email address in a specific
-- location.
--
-- @since 0.1.0.0
data Comment = Comment CommentLoc CommentContent
  deriving (Show, Eq)

-- | Prism for working with a 'Comment'.
--
-- @since 0.1.0.0
_Comment :: Prism' Comment (CommentLoc, CommentContent)
_Comment =
  prism'
    (uncurry Comment)
    (\(Comment loc cc) -> Just (loc, cc))

-- | Lens for working with comment locations.
--
-- @since 0.1.0.0
commentLoc :: Lens' Comment CommentLoc
commentLoc =
  lens
    (\(Comment loc _) -> loc)
    (\(Comment _ cc) loc -> Comment loc cc)

-- | Lens for working with comment contents.
--
-- @since 0.1.0.0
commentContent :: Lens' Comment CommentContent
commentContent =
  lens
    (\(Comment _ cc) -> cc)
    (\(Comment loc _) cc -> Comment loc cc)

-- | The location where a comment was parsed or where it should be
-- rendered.
--
-- @since 0.1.0.0
data CommentLoc
  = -- | Just before the 'DisplayName'.
    BeforeDisplayName
  | -- | Just after the 'DisplayName' but before the address.
    AfterDisplayName
  | -- | Before the 'LocalPart' of the address.
    BeforeLocalPart
  | -- | After the 'Domain'.
    AfterDomain
  | -- | After the complete address.
    AfterAddress
  deriving (Show, Eq)

-- | Text that can appear in a comment.
--
-- @since 0.1.0.0
newtype CommentContent = CC
  { commentContentText :: Text
  }
  deriving newtype (Show, Eq, Semigroup)
