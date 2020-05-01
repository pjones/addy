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
module Addy.Internal.Types
  ( Email (..),
    LocalPart (..),
    DomainName (..),
    AddressLiteral (..),
    Atom' (..),
    Atom,
    DisplayName (..),
    ObsPhraseTail (..),
    Comment (..),
    Comments (..),
    Content (..),
    text,
    dots,
    aleft,
    aright,
  )
where

import qualified Data.Text as Text
import Net.IP (IP)
import Relude.Extra.Lens

data Content
  = CleanText Text
  | ObsText Text
  deriving (Eq, Show)

instance Semigroup Content where
  (<>) (CleanText x) (CleanText y) = CleanText (x <> y)
  (<>) x y = ObsText ((x ^. text) <> (y ^. text))

instance Monoid Content where
  mempty = CleanText mempty

-- | A lens for accessing the text content of a 'Content'.
--
-- @since 0.1.0.0
text :: Lens' Content Text
text = lens getter setter
  where
    getter = \case
      CleanText t -> t
      ObsText t -> t
    setter = \case
      CleanText {} -> CleanText
      ObsText {} -> ObsText

-- | FIXME: Write description for dots
--
-- @since 0.1.0.0
dots :: Lens' Content [Content]
dots = lens getter setter
  where
    getter = \case
      CleanText t -> map CleanText (split t)
      ObsText t -> map ObsText (split t)
    setter = \case
      CleanText _ -> CleanText . join
      ObsText _ -> ObsText . join
    split = Text.splitOn "."
    join = Text.intercalate "." . map (^. text)

data Comment = Comment Content (Maybe Content)
  deriving (Eq, Show)

-- | CFWS.
data Comments
  = Comments (NonEmpty (Maybe Content, Comment)) (Maybe Content)
  | FoldingSpace Content
  deriving (Eq, Show)

data ObsPhraseTail
  = ObsPhraseWord Atom
  | ObsPhraseDot
  | ObsPhraseComment Comments
  deriving (Eq, Show)

data DisplayName
  = DisplayAtoms (NonEmpty Atom)
  | DisplayObsolete Atom [ObsPhraseTail]
  deriving (Eq, Show)

data Atom' a
  = Atom (Maybe Comments) a (Maybe Comments)
  | AtomQuoted (Maybe Comments) a (Maybe Comments)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | A lens for access the left-hand comment.
--
-- @since 0.1.0.0
aleft :: Lens' (Atom' a) (Maybe Comments)
aleft = lens getter setter
  where
    getter = \case
      Atom c _ _ -> c
      AtomQuoted c _ _ -> c
    setter = \case
      Atom _ x y -> \c -> Atom c x y
      AtomQuoted _ x y -> \c -> AtomQuoted c x y

-- | A lens for access the right-hand comment.
--
-- @since 0.1.0.0
aright :: Lens' (Atom' a) (Maybe Comments)
aright = lens getter setter
  where
    getter = \case
      Atom _ _ c -> c
      AtomQuoted _ _ c -> c
    setter = \case
      Atom y x _ -> Atom y x
      AtomQuoted y x _ -> AtomQuoted y x

type Atom = Atom' Content

data LocalPart
  = -- | Modern local part made up of a single atom.
    LocalAtom Atom
  | -- | An obsolete local part where the list of atoms are separated
    -- by dots.
    LocalObsolete (NonEmpty Atom)
  deriving (Eq, Show)

data AddressLiteral
  = IpAddress IP
  | TaggedAddress Content Content
  | AddressLiteral Content
  deriving (Eq, Show)

data DomainName
  = DomainAtom Atom
  | DomainLiteral AddressLiteral (Maybe Comments)
  | DomainObsolete (NonEmpty Atom)
  deriving (Eq, Show)

-- | A parsed email address.
data Email
  = NameAddr
      (Maybe DisplayName)
      (Maybe Comments)
      LocalPart
      DomainName
      (Maybe Comments)
  | AddrSpec LocalPart DomainName
  deriving (Eq, Show)
