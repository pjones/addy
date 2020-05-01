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
module Addy.Internal.Render
  ( Mode (..),
    render,
  )
where

import Addy.Internal.Types
import Data.Char (isAscii, isControl)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TB
import qualified Net.IP as IP
import Relude.Extra.Lens

data Mode
  = Full
  | Simple

class Render a where
  render :: Mode -> a -> TB.Builder

instance Render a => Render (Maybe a) where
  render mode = \case
    Nothing -> mempty
    Just x -> render mode x

instance Render a => Render (NonEmpty a) where
  render mode = foldMap (render mode)

instance Render Content where
  render _ = \case
    CleanText t -> TB.fromText t
    ObsText t -> TB.fromText (clean t)
    where
      clean = Text.filter $ \c ->
        not (isAscii c)
          || not (isControl c)

instance Render Email where
  render mode = \case
    NameAddr dn c0 localPart domain c1 ->
      case mode of
        Simple ->
          mconcat
            [ render mode localPart,
              TB.singleton '@',
              render mode domain
            ]
        Full ->
          mconcat
            [ render mode dn,
              render mode c0,
              TB.singleton '<',
              render mode localPart,
              TB.singleton '@',
              render mode domain,
              TB.singleton '>',
              render mode c1
            ]
    AddrSpec localPart domain ->
      mconcat
        [ render mode localPart,
          TB.singleton '@',
          render mode domain
        ]

instance Render LocalPart where
  render mode = \case
    LocalAtom atom ->
      render mode (atom & aright .~ Nothing)
    LocalObsolete atoms ->
      renderObsDotAtoms mode $
        fmap (aright .~ Nothing) atoms

instance Render DomainName where
  render mode = \case
    DomainAtom atom ->
      render mode (atom & aleft .~ Nothing)
    DomainLiteral lit cs ->
      render mode lit
        <> render mode cs
    DomainObsolete atoms ->
      renderObsDotAtoms mode $
        fmap (aleft .~ Nothing) atoms

instance Render AddressLiteral where
  render mode lit =
    mconcat
      [ TB.singleton '[',
        go lit,
        TB.singleton ']'
      ]
    where
      go :: AddressLiteral -> TB.Builder
      go = \case
        IpAddress ip ->
          TB.fromText $
            IP.case_ (const Text.empty) (const "IPv6:") ip
              <> IP.encode ip
        TaggedAddress tag body ->
          render mode tag
            <> TB.singleton ':'
            <> render mode body
        AddressLiteral t ->
          render mode t
            & escapeB
              ( \c ->
                  c == '[' || c == ']'
              )

instance Render a => Render (Atom' a) where
  render mode = \case
    Atom c0 x c1 ->
      mconcat
        [ render mode c0,
          render mode x,
          render mode c1
        ]
    AtomQuoted c0 x c1 ->
      mconcat
        [ render mode c0,
          TB.singleton '"',
          esc (render mode x),
          TB.singleton '"',
          render mode c1
        ]
    where
      esc = escapeB $ \c ->
        c == '"'

instance Render DisplayName where
  render mode = case mode of
    Simple -> const mempty
    Full -> \case
      DisplayAtoms atoms ->
        render mode atoms
      DisplayObsolete atom _ ->
        render mode atom

instance Render Comment where
  render mode = case mode of
    Simple -> mempty
    Full -> \case
      Comment comment ending ->
        inComment comment
          <> render mode ending
    where
      inComment :: Content -> TB.Builder
      inComment content =
        mconcat
          [ TB.singleton '(',
            esc (render mode content),
            TB.singleton ')'
          ]
      esc = escapeB $ \c ->
        c == '('
          || c == ')'

instance Render Comments where
  render mode c = case mode of
    Simple ->
      mempty
    Full ->
      case c of
        Comments cs t ->
          foldMap (\(m, c) -> render mode m <> render mode c) cs
            <> render mode t
        FoldingSpace _ ->
          TB.fromText " "

-- | Render obsolete atoms that should be separated with dots.
renderObsDotAtoms :: Mode -> NonEmpty Atom -> TB.Builder
renderObsDotAtoms mode =
  NonEmpty.toList
    >>> map (render mode)
    >>> intersperse (TB.fromText ".")
    >>> mconcat

-- | FIXME: Write description for escapeB
--
-- @since 0.1.0.0
escapeB ::
  (Char -> Bool) -> TB.Builder -> TB.Builder
escapeB f =
  TB.toLazyText
    >>> escape f
    >>> TB.fromLazyText

-- | Escape characters for which the given function return true.
--
-- @since 0.1.0.0
escape :: (Char -> Bool) -> LText -> LText
escape f = LText.foldl' go LText.empty
  where
    go :: LText -> Char -> LText
    go t c
      | c == '\\' = t <> "\\\\"
      | f c = t <> one '\\' <> one c
      | otherwise = t <> one c
