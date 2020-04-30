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
    Mode (..),
  )
where

import Net.IP (IP)

data Comment
  = Comment Text
  | FoldableSpace Text
  deriving (Eq, Show)

data ObsPhraseTail
  = ObsPhraseWord Atom
  | ObsPhraseDot
  | ObsPhraseComment Comment
  deriving (Eq, Show)

data DisplayName
  = DisplayAtoms (NonEmpty Atom)
  | DisplayObsolete Atom [ObsPhraseTail]
  deriving (Eq, Show)

data Atom' a
  = Atom (Maybe Comment) a (Maybe Comment)
  | AtomQuoted (Maybe Comment) a (Maybe Comment)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

type Atom = Atom' Text

data LocalPart
  = -- | Modern local part made up of a single atom.
    LocalAtom Atom
  | -- | An atom where the text was quoted.
    LocalQuoted Atom
  | -- | An obsolete local part where the list of atoms are separated
    -- by dots.
    LocalObsolete (NonEmpty Atom)
  deriving (Eq, Show)

data AddressLiteral
  = IpAddress IP
  | TaggedAddress Text Text
  | AddressLiteral Text
  deriving (Eq, Show)

data DomainName
  = DomainAtom Atom
  | DomainLiteral AddressLiteral
  | DomainObsolete (NonEmpty Atom)
  deriving (Eq, Show)

-- | A parsed email address.
data Email
  = NameAddr
      (Maybe DisplayName)
      (Maybe Comment)
      LocalPart
      DomainName
      (Maybe Comment)
  | AddrSpec LocalPart DomainName
  deriving (Eq, Show)

-- |
data Mode
  = Strict
  | Lenient
  deriving (Eq, Show)
