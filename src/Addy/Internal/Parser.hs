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
-- Internal parsing functions.
module Addy.Internal.Parser
  ( Mode (..),
    Atom (..),
    parse,
    parseWithMode,
    nameAddr,
    addrSpec,
    localPartP,
    domainP,
    displayNameP,
    word,
    atom,
    dotAtom,
    dotAtomLh,
    dotAtomRh,
    quoted,
    quotedLh,
    cfws,
  )
where

import Addy.Internal.Char
import Addy.Internal.Types
import Addy.Internal.Validation
import Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as Atto
import Data.Foldable
import qualified Data.Text as Text
import qualified Net.IP as IP
import qualified Net.IPv4 as IP4
import qualified Net.IPv6 as IP6
import qualified Validation

-- | Parsing mode.
--
-- @since 0.1.0.0
data Mode
  = -- | Only support non-obsoleted addresses.
    Strict
  | -- | Include support for obsolete addresses.
    Lenient
  deriving (Eq, Show)

-- | RFC 5322 @atom@.
--
-- @since 0.1.0.0
data Atom = Atom (Maybe CommentContent) Text (Maybe CommentContent)
  deriving (Show)

instance Semigroup Atom where
  (<>) (Atom x0 y0 z0) (Atom x1 y1 z1) =
    Atom (x0 <> x1) (y0 <> y1) (z0 <> z1)

instance Monoid Atom where
  mempty = Atom Nothing mempty Nothing

-- | FIXME: Write description for atomJoin
--
-- @since 0.1.0.0
atomJoin :: Foldable t => Char -> t Atom -> Atom
atomJoin sep as
  | null as = mempty
  | otherwise = foldr1 go as
  where
    go :: Atom -> Atom -> Atom
    go (Atom c0 t0 c1) (Atom c2 t1 c3) =
      Atom (go' c0 c2) (t0 <> one sep <> t1) (go' c1 c3)
    go' :: Maybe CommentContent -> Maybe CommentContent -> Maybe CommentContent
    go' (Just x) (Just y) = Just (x <> CC (one ' ') <> y)
    go' x y = x <|> y

-- | An email address parser.
--
-- @since 0.1.0.0
parse :: Mode -> Atto.Parser EmailAddr
parse m = cleanComments <$> (nameAddr m <|> addrSpec m)
  where
    -- Since white space is allowed all over the place, filter out
    -- comments that are just white space.
    cleanComments :: EmailAddr -> EmailAddr
    cleanComments addr@EmailAddr {_comments} =
      addr
        { _comments =
            filter
              ( \(Comment _ (CC t)) -> not $ Text.null (Text.strip t)
              )
              _comments
        }

-- | Run the parser and then validate the resulting address.
--
-- @since 0.1.0.0
parseWithMode :: Mode -> Text -> Either (NonEmpty Error) EmailAddr
parseWithMode mode text = do
  addr <-
    first (toText >>> ParserFailedError >>> one) $
      Atto.parseOnly
        ( parse mode <* (Atto.endOfInput <?> "unparsed input")
        )
        text
  case validateEmailAddr addr of
    Validation.Success ea -> Right ea
    Validation.Failure es -> Left es

-- | Parse email addresses in the @name-addr@ format.
--
-- @since 0.1.0.0
nameAddr :: Mode -> Atto.Parser EmailAddr
nameAddr mode = do
  dp <- optional (displayNameP mode)
  c0 <- optional (cfws mode)
  _ <- Atto.char '<'
  (c1, lp) <- localPartP mode <* Atto.char '@'
  (dn, c2) <- domainP mode
  _ <- Atto.char '>'
  c3 <- optional (cfws mode)
  let (dpc0, dpt, dpc1) = case dp of
        Nothing -> (Nothing, Nothing, Nothing)
        Just (Atom x y z) -> (x, Just (DP y), z)
  pure $
    EmailAddr
      { _displayName = dpt,
        _localPart = lp,
        _domain = dn,
        _comments =
          catMaybes
            [ Comment BeforeDisplayName <$> dpc0,
              Comment AfterDisplayName <$> dpc1,
              Comment AfterDisplayName <$> c0,
              Comment BeforeLocalPart <$> c1,
              Comment AfterDomain <$> c2,
              Comment AfterAddress <$> c3
            ]
      }

-- | Parse email addresses in the @addr-spec@ format.
--
-- @since 0.1.0.0
addrSpec :: Mode -> Atto.Parser EmailAddr
addrSpec mode = do
  (c0, lp) <- localPartP mode <* Atto.char '@'
  (dn, c1) <- domainP mode
  pure $
    EmailAddr
      { _displayName = Nothing,
        _localPart = lp,
        _domain = dn,
        _comments =
          catMaybes
            [ Comment BeforeLocalPart <$> c0,
              Comment AfterDomain <$> c1
            ]
      }

-- | Parse the @local-part@ of an email address.
--
-- RFC 5322 §3.4.1
--
-- > local-part = dot-atom / quoted-string / obs-local-part
--
-- @since 0.1.0.0
localPartP :: Mode -> Atto.Parser (Maybe CommentContent, LocalPart)
localPartP mode = go <?> "local part"
  where
    go =
      case mode of
        Strict -> do
          Atom c0 t c1 <- (dotAtomLh <* thenAt) <|> (quotedLh <* thenAt)
          pure (c0 <> c1, LP t)
        Lenient -> do
          -- Obsolete comes before quoted since the obsolete syntax allows
          -- multiple quoted strings separated by dots.
          Atom c0 t c1 <-
            Atto.choice
              [ dotAtom mode <* thenAt,
                obsLocalPart <* thenAt,
                quoted mode <* thenAt
              ]
          pure (c0 <> c1, LP t)
    -- > obs-local-part = word *("." word)
    obsLocalPart :: Atto.Parser Atom
    obsLocalPart = do
      t0 <- word mode
      ts <- many (Atto.char '.' *> word mode)
      pure (atomJoin '.' (t0 : ts))
    thenAt :: Atto.Parser ()
    thenAt =
      Atto.peekChar'
        >>= bool empty pass . (== '@')

-- | Domain name parser.
--
-- @since 0.1.0.0
domainP :: Mode -> Atto.Parser (Domain, Maybe CommentContent)
domainP mode = go <?> "domain name"
  where
    go =
      case mode of
        Strict ->
          domainNameP
            <|> domainLiteralP (fws mode $> CC (one ' '))
        Lenient ->
          obsDomainP
            <|> domainNameP
            <|> domainLiteralP (cfws mode)
    domainNameP :: Atto.Parser (Domain, Maybe CommentContent)
    domainNameP = do
      Atom c0 t c1 <- dotAtomRh
      pure (Domain $ DN t, c0 <> c1)
    -- > domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
    domainLiteralP ::
      Atto.Parser CommentContent ->
      Atto.Parser (Domain, Maybe CommentContent)
    domainLiteralP lh = do
      c0 <- optional lh
      t <- addressLiteral mode
      c1 <- optional (cfws mode)
      pure (DomainLiteral t, c0 <> c1)
    -- > obs-domain = atom *("." atom)
    obsDomainP :: Atto.Parser (Domain, Maybe CommentContent)
    obsDomainP = do
      t0 <- atom mode
      ts <- many (Atto.char '.' *> atom mode)
      let Atom c0 t c1 = atomJoin '.' (t0 : ts)
      pure (Domain $ DN t, c0 <> c1)

-- | Parse a display name.
--
-- @since 0.1.0.0
displayNameP :: Mode -> Atto.Parser Atom
displayNameP mode =
  case mode of
    Strict -> phrase
    Lenient -> phrase <|> obsPhrase
  where
    phrase = (<?> "display name") $ do
      -- Always strict since in lenient mode we'll fall back to
      -- obsolete mode anyways.
      w0 <- word Strict
      ws <- many (word Strict)
      pure (atomJoin ' ' (w0 : ws))
    obsPhrase = (<?> "obsolete display name") $ do
      w0 <- word mode
      ws <-
        many
          ( word mode
              <|> Atom Nothing <$> (Atto.char '.' <&> one) <*> pure Nothing
              <|> Atom <$> (cfws mode <&> Just) <*> pure mempty <*> pure Nothing
          )
      pure (atomJoin ' ' (w0 : ws))

-- | An atom or quoted string.
--
-- > word = atom / quoted-string
--
-- @since 0.1.0.0
word :: Mode -> Atto.Parser Atom
word mode = atom mode <|> quoted mode

-- | Parse an unquoted atom.
--
-- > atom            =   [CFWS] 1*atext [CFWS]
--
-- @since 0.1.0.0
atom :: Mode -> Atto.Parser Atom
atom mode =
  Atom
    <$> optional (cfws mode)
    <*> atextP
    <*> optional (cfws mode)

-- | Parse an unquoted atom that is allowed to contain periods.
--
-- @since 0.1.0.0
dotAtom' ::
  Atto.Parser CommentContent ->
  Atto.Parser CommentContent ->
  Atto.Parser Atom
dotAtom' lh rh = do
  c0 <- optional lh
  t0 <- atextP
  ts <- many (Atto.char '.' *> atextP)
  c1 <- optional rh
  pure (Atom c0 (Text.intercalate "." (t0 : ts)) c1)

-- | RFC 5322 @dot-atom@.
--
-- @since 0.1.0.0
dotAtom :: Mode -> Atto.Parser Atom
dotAtom mode = dotAtom' (cfws mode) (cfws mode)

-- | Strict @dot-atom-lh@ from RFC 5322 errata.
--
-- > dot-atom-lh = [CFWS] dot-atom-text [FWS]
--
-- @since 0.1.0.0
dotAtomLh :: Atto.Parser Atom
dotAtomLh =
  dotAtom'
    (cfws Strict)
    (CC <$> (fws Strict $> one ' '))

-- | Strict @dot-atom-rh@ from RFC 5322 errata.
--
-- > dot-atom-rh = [FWS] dot-atom-text [CFWS]
--
-- @since 0.1.0.0
dotAtomRh :: Atto.Parser Atom
dotAtomRh =
  dotAtom'
    (CC <$> (fws Strict $> one ' '))
    (cfws Strict)

-- | Is a character allowed in an atom?
--
-- RFC 5322 §3.2.3
--
-- @since 0.1.0.0
atextP :: Atto.Parser Text
atextP = Atto.takeWhile1 atext

-- | A quoted string.
--
-- RFC5322 §3.2.4
--
-- > qtext           =   %d33 /             ; Printable US-ASCII
-- >                     %d35-91 /          ;  characters not including
-- >                     %d93-126 /         ;  "\" or the quote character
-- >                     obs-qtext
-- >
-- > qcontent        =   qtext / quoted-pair
-- >
-- > quoted-string   =   [CFWS]
-- >                     DQUOTE *([FWS] qcontent) [FWS] DQUOTE
-- >                     [CFWS]
-- >
-- > obs-qtext       =   obs-NO-WS-CTL
-- >
--
-- RFC 6532 §3.2
--
-- > qtext   =/  UTF8-non-ascii
--
-- RFC 5322 errata item 3135 <https://www.rfc-editor.org/errata/eid3135>
--
--
-- > quoted-string   =   [CFWS]
-- >                     DQUOTE ((1*([FWS] qcontent) [FWS]) / FWS) DQUOTE
-- >                     [CFWS]
--
-- This is the rule we use since it's consistent with the text of the RFC.
--
-- @since 0.1.0.0
quoted :: Mode -> Atto.Parser Atom
quoted mode = quoted' (cfws mode) (cfws mode) mode

-- | General-purpose quoted-string parser.
--
-- @since 0.1.0.0
quoted' ::
  Atto.Parser CommentContent ->
  Atto.Parser CommentContent ->
  Mode ->
  Atto.Parser Atom
quoted' lh rh mode = (<?> "quoted content") $ do
  c0 <- optional lh
  _ <- Atto.char '"'
  t <- Atto.many1 ((<>) <$> fws' <*> qcontent)
  w <- fws'
  _ <- Atto.char '"'
  c1 <- optional rh
  pure (Atom c0 (mconcat t <> w) c1)
  where
    -- Characters that are allowed in the quotes:
    qcontent :: Atto.Parser Text
    qcontent = qtextP <|> quotedPairP mode
    qtextP :: Atto.Parser Text
    qtextP = case mode of
      Strict -> Atto.takeWhile1 qtext
      Lenient ->
        Atto.takeWhile1 (\c -> qtext c || qtextObs c)
          <&> Text.filter (not . qtextObs)
    fws' = (fws mode $> one ' ') <|> pure mempty

-- | Strict @quoted-string-lh@ from RFC 5322 errata.
--
-- @since 0.1.0.0
quotedLh :: Atto.Parser Atom
quotedLh =
  quoted'
    (cfws Strict)
    (CC <$> (fws Strict $> one ' '))
    Strict

-- | Parse backslash escapes:
--
-- @since 0.1.0.0
quotedPairP :: Mode -> Atto.Parser Text
quotedPairP mode = go <?> "quoted char"
  where
    go = Atto.char '\\' *> allowed
    allowed = case mode of
      Strict ->
        Atto.satisfy quotedPair
          <&> one
      Lenient ->
        Atto.satisfy (\c -> quotedPair c || quotedPairObs c)
          <&> (one >>> Text.filter (not . quotedPairObs))

-- | Comments and folding white space.
--
-- > ctext           =   %d33-39 /          ; Printable US-ASCII
-- >                     %d42-91 /          ;  characters not including
-- >                     %d93-126 /         ;  "(", ")", or "\"
-- >                     obs-ctext
-- >
-- > obs-ctext       =   obs-NO-WS-CTL
-- >
-- > ccontent        =   ctext / quoted-pair / comment
-- >
-- > comment         =   "(" *([FWS] ccontent) [FWS] ")"
-- >
-- > CFWS            =   (1*([FWS] comment) [FWS]) / FWS
--
-- @since 0.1.0.0
cfws :: Mode -> Atto.Parser CommentContent
cfws mode =
  (<?> "comment or space")
    (cfws' <|> (CC <$> fws mode))
  where
    cfws' :: Atto.Parser CommentContent
    cfws' = do
      cs <- Atto.many1 (fws' *> comment) <* fws'
      pure (CC $ mconcat cs)
    comment :: Atto.Parser Text
    comment = do
      _ <- Atto.char '('
      ts <- many (fws' *> (mconcat <$> Atto.many1 ccontent)) <* fws'
      _ <- Atto.char ')'
      pure (Text.intercalate " " ts)
    ccontent :: Atto.Parser Text
    ccontent = ctextP <|> quotedPairP mode <|> comment
    ctextP :: Atto.Parser Text
    ctextP = case mode of
      Strict ->
        Atto.takeWhile1 ctext
      Lenient ->
        Atto.takeWhile1 (\c -> ctext c || ctextObs c)
          <&> Text.filter (not . ctextObs)
    fws' :: Atto.Parser ()
    fws' = void (optional (fws mode))

-- | Folding white space.
--
-- > FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
-- >                                        ; Folding white space
-- >
-- > obs-FWS         =   1*WSP *(CRLF 1*WSP)
--
-- @since 0.1.0.0
fws :: Mode -> Atto.Parser Text
fws = \case
  Strict -> do
    w0 <- (Atto.takeWhile wsp <* crlf) <|> pure Text.empty
    w1 <- Atto.takeWhile1 wsp
    pure (w0 <> w1)
  Lenient -> do
    w0 <- Atto.takeWhile1 wsp
    ws <- many (crlf *> Atto.takeWhile1 wsp)
    pure (w0 <> mconcat ws)
  where
    crlf = Atto.string "\r\n"

-- | Parse a domain/address literal.
--
-- > dtext           =   %d33-90 /          ; Printable US-ASCII
-- >                     %d94-126 /         ;  characters not including
-- >                     obs-dtext          ;  "[", "]", or "\"
-- > obs-dtext       =   obs-NO-WS-CTL / quoted-pair
--
-- @since 0.1.0.0
addressLiteral :: Mode -> Atto.Parser AddressLiteral
addressLiteral mode =
  (<?> "address literal")
    $ Atto.choice
    $ map
      wrap
      [ IpAddressLiteral . IP.fromIPv6 <$> (Atto.string "IPv6:" *> IP6.parser),
        TaggedAddressLiteral <$> tag <*> (Atto.char ':' *> lit),
        IpAddressLiteral . IP.fromIPv4 <$> IP4.parser,
        AddressLiteral <$> lit
      ]
  where
    wrap :: Atto.Parser a -> Atto.Parser a
    wrap p =
      Atto.char '['
        *> p
        <* optional (fws mode)
        <* Atto.char ']'
    tag :: Atto.Parser AddressTag
    tag = Atto.takeWhile1 (\c -> c /= ':' && dtext c) <&> AT
    lit :: Atto.Parser Literal
    lit =
      Lit . mconcat
        <$> many
          ( do
              f0 <- fws'
              ts <- dtextP
              f1 <- fws'
              pure (f0 <> ts <> f1)
          )
    dtextP :: Atto.Parser Text
    dtextP =
      case mode of
        Strict ->
          Atto.takeWhile1 dtext
        Lenient ->
          -- Allow obsolete syntax, but don't capture it.
          mconcat
            <$> Atto.many1
              ( ( Atto.takeWhile1 (\c -> dtext c || obsNoWsCtl c)
                    <&> Text.filter (not . obsNoWsCtl)
                )
                  <|> (quotedPairP mode $> one '-')
              )
    fws' :: Atto.Parser Text
    fws' = fws mode $> one ' ' <|> pure mempty
