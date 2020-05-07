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
-- Internal functions to render an 'EmailAddr' to a 'TB.Builder'.
module Addy.Internal.Render
  ( Mode (..),
    render,
    renderToText,
    renderAddrSpec,
    renderDisplayName,
    renderComments,
  )
where

import Addy.Internal.Char
import Addy.Internal.Types
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TB
import qualified Net.IP as IP

-- | Render mode.
--
-- @since 0.1.0.0
data Mode
  = -- | Render the entire email address, including the optional
    -- display name and comments.
    Full
  | -- | Only render the simplest form of the email address.  Only the
    -- 'LocalPart' and 'Domain' are rendered in this mode.
    Short

-- | Render an email address.
--
-- @since 0.1.0.0
render :: Mode -> EmailAddr -> TB.Builder
render = \case
  Short ->
    renderAddrSpec Short
  Full -> \addr@EmailAddr {..} ->
    case _displayName of
      Nothing ->
        renderAddrSpec Full addr
          <> renderComments Full AfterAddress _comments
      Just name ->
        mconcat
          [ renderComments Full BeforeDisplayName _comments,
            renderDisplayName name <> TB.singleton ' ',
            renderComments Full AfterDisplayName _comments,
            TB.singleton '<' <> renderAddrSpec Full addr <> TB.singleton '>',
            renderComments Full AfterAddress _comments
          ]

-- | Render an email address in @addr-spec@ format.
--
-- @since 0.1.0.0
renderAddrSpec :: Mode -> EmailAddr -> TB.Builder
renderAddrSpec mode EmailAddr {..} =
  mconcat
    [ renderComments mode BeforeLocalPart _comments,
      lp _localPart <> TB.singleton '@' <> dn _domain,
      renderComments mode AfterDomain _comments
    ]
  where
    lp :: LocalPart -> TB.Builder
    lp (LP t)
      | mustQuoteLocalPart t = wrap '"' '"' t
      | otherwise = TB.fromText t
    dn :: Domain -> TB.Builder
    dn = \case
      Domain (DN t) ->
        TB.fromText t
      DomainLiteral lit ->
        wrap '[' ']' $ case lit of
          IpAddressLiteral ip ->
            if IP.isIPv6 ip
              then "IPv6:" <> IP.encode ip
              else IP.encode ip
          TaggedAddressLiteral (AT tag) (Lit body) ->
            tag <> ":" <> body
          AddressLiteral (Lit body) ->
            body

-- | Render a display name.
--
-- @since 0.1.0.0
renderDisplayName :: DisplayName -> TB.Builder
renderDisplayName (DP t)
  | Text.all (\c -> atext c || wsp c) t =
    TB.fromText t
  | otherwise =
    wrap '"' '"' t

-- | Render comments that have the given 'CommentLoc'.  The comment
-- location is also used to decide where to introduce white space.
--
-- @since 0.1.0.0
renderComments :: Mode -> CommentLoc -> [Comment] -> TB.Builder
renderComments Short _ _ = mempty
renderComments Full loc cs =
  case go (== loc) cs of
    Nothing -> mempty
    Just tb -> case loc of
      BeforeDisplayName -> TB.singleton ' ' <> tb
      AfterDisplayName -> tb <> TB.singleton ' '
      BeforeLocalPart -> tb <> TB.singleton ' '
      AfterDomain -> TB.singleton ' ' <> tb
      AfterAddress -> TB.singleton ' ' <> tb
  where
    go :: (CommentLoc -> Bool) -> [Comment] -> Maybe TB.Builder
    go f cs =
      filter (\(Comment loc (CC t)) -> f loc && not (Text.null t)) cs
        & map (\(Comment _ (CC t)) -> t)
        & Text.intercalate " "
        & \t ->
          if Text.null t
            then Nothing
            else Just $ wrap '(' ')' t

-- | Render the given address as text.
--
-- @since 0.1.0.0
renderToText :: Mode -> EmailAddr -> Text
renderToText m =
  render m
    >>> TB.toLazyText
    >>> toStrict

-- | Wrap and quote some text.
--
-- @since 0.1.0.0
wrap :: Char -> Char -> Text -> TB.Builder
wrap lh rh t =
  mconcat
    [ TB.singleton lh,
      Text.foldl' escape mempty t,
      TB.singleton rh
    ]
  where
    escape :: TB.Builder -> Char -> TB.Builder
    escape tb c
      | c == lh || c == rh || c == '\\' =
        tb <> TB.singleton '\\' <> TB.singleton c
      | otherwise =
        tb <> TB.singleton c

-- | 'True' if the give text, when used as the @local-part@ of an
-- email address must be wrapped in quotation marks.
--
-- @since 0.1.0.0
mustQuoteLocalPart :: Text -> Bool
mustQuoteLocalPart name =
  Text.any
    ( \c ->
        c == '"'
          || c == '\\'
          || c == ')'
          || c == '('
          || c == '@'
          || wsp c
    )
    name
    || Text.isPrefixOf "." name
    || Text.isSuffixOf "." name
    || Text.isInfixOf ".." name
    || Text.null name -- Yes, this is totally legit.
