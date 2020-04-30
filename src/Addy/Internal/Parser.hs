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
module Addy.Internal.Parser
  ( parse,
    nameAddr,
    addrSpec,
    localPart,
    domain,
    displayName,
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

import Addy.Internal.Types
import Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as Atto
import Data.Char (isAlphaNum, isPrint)
import qualified Data.Text as Text
import qualified Net.IP as IP
import qualified Net.IPv4 as IP4
import qualified Net.IPv6 as IP6

-- | An email address parser.
--
-- @since 0.1.0.0
parse :: Mode -> Atto.Parser Email
parse m = nameAddr m <|> addrSpec m

-- | Parse email addresses in the @name-addr@ format.
--
-- @since 0.1.0.0
nameAddr :: Mode -> Atto.Parser Email
nameAddr mode = do
  dp <- optional (displayName mode)
  c0 <- optional (cfws mode)
  _ <- Atto.char '<'
  lp <- localPart mode <* Atto.char '@'
  dn <- domain mode
  _ <- Atto.char '>'
  c1 <- optional (cfws mode)
  pure (NameAddr dp c0 lp dn c1)

-- | Parse email addresses in the @addr-spec@ format.
--
-- @since 0.1.0.0
addrSpec :: Mode -> Atto.Parser Email
addrSpec mode =
  AddrSpec
    <$> (localPart mode <* Atto.char '@')
    <*> domain mode

-- | Parse the @local-part@ of an email address.
--
-- RFC 5322 §3.4.1
--
-- > local-part = dot-atom / quoted-string / obs-local-part
--
-- @since 0.1.0.0
localPart :: Mode -> Atto.Parser LocalPart
localPart mode = go <?> "local part"
  where
    go =
      case mode of
        Strict ->
          (LocalAtom <$> dotAtomLh <* thenAt)
            <|> (LocalQuoted <$> quotedLh <* thenAt)
        Lenient ->
          -- Obsolete comes before quoted since the obsolete syntax allows
          -- multiple quoted strings separated by dots.
          (LocalAtom <$> dotAtom mode <* thenAt)
            <|> obsLocalPart <* thenAt
            <|> (LocalQuoted <$> quoted mode <* thenAt)
    -- > obs-local-part = word *("." word)
    obsLocalPart :: Atto.Parser LocalPart
    obsLocalPart = do
      t0 <- word mode
      ts <- many (Atto.char '.' *> word mode)
      pure (LocalObsolete $ t0 :| ts)
    thenAt :: Atto.Parser ()
    thenAt =
      Atto.peekChar'
        >>= bool empty pass . (== '@')

-- | Domain name parser.
--
-- @since 0.1.0.0
domain :: Mode -> Atto.Parser DomainName
domain mode = do
  name <- go <?> "domain name"
  check name
  pure name
  where
    go =
      case mode of
        Strict ->
          (DomainAtom <$> dotAtomRh)
            <|> domainLiteral (fws mode)
        Lenient ->
          obsDomain
            <|> (DomainAtom <$> dotAtom mode)
            <|> domainLiteral (cfws mode)
    check = \case
      DomainAtom atom -> traverse_ (traverse_ checkText . Text.splitOn ".") atom
      DomainLiteral _ -> pass
      DomainObsolete atoms -> traverse_ (traverse_ checkText) atoms
    checkText text =
      if "-" `Text.isPrefixOf` text || "-" `Text.isSuffixOf` text
        then fail "must not start or end with a hypen"
        else pass
    -- > domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
    domainLiteral lh = do
      _ <- optional lh
      t <- addressLiteral mode
      _ <- optional (cfws mode)
      pure (DomainLiteral t)
    -- > obs-domain = atom *("." atom)
    obsDomain = do
      t0 <- atom mode
      ts <- many (Atto.char '.' *> atom mode)
      pure (DomainObsolete (t0 :| ts))

-- | Parse a display name.
--
-- @since 0.1.0.0
displayName :: Mode -> Atto.Parser DisplayName
displayName mode =
  case mode of
    Strict -> phrase'
    Lenient -> phrase' <|> obsPhrase
  where
    phrase' = (<?> "display name") $ do
      w0 <- word mode
      ws <- many (word mode)
      pure $ DisplayAtoms (w0 :| ws)
    obsPhrase = (<?> "obsolete display name") $ do
      w0 <- word mode
      ws <-
        many
          ( (ObsPhraseWord <$> word mode)
              <|> (Atto.char '.' $> ObsPhraseDot)
              <|> (ObsPhraseComment <$> cfws mode)
          )
      pure (DisplayObsolete w0 ws)

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
    <*> Atto.takeWhile1 (atext mode)
    <*> optional (cfws mode)

-- | Parse an unquoted atom that is allowed to contain periods.
--
-- @since 0.1.0.0
dotAtom' ::
  Atto.Parser Comment ->
  Atto.Parser Comment ->
  Mode ->
  Atto.Parser Atom
dotAtom' lh rh mode = do
  c0 <- optional lh
  t0 <- Atto.takeWhile1 (atext mode)
  ts <- many (Atto.char '.' *> Atto.takeWhile1 (atext mode))
  c1 <- optional rh
  pure (Atom c0 (Text.intercalate "." (t0 : ts)) c1)

-- | RFC 5322 @dot-atom@.
dotAtom :: Mode -> Atto.Parser Atom
dotAtom mode = dotAtom' (cfws mode) (cfws mode) mode

-- | Strict @dot-atom-lh@ from RFC 5322 errata.
--
-- > dot-atom-lh = [CFWS] dot-atom-text [FWS]
--
-- @since 0.1.0.0
dotAtomLh :: Atto.Parser Atom
dotAtomLh = dotAtom' (cfws Strict) (fws Strict) Strict

-- | Strict @dot-atom-rh@ from RFC 5322 errata.
--
-- > dot-atom-rh = [FWS] dot-atom-text [CFWS]
--
-- @since 0.1.0.0
dotAtomRh :: Atto.Parser Atom
dotAtomRh = dotAtom' (fws Strict) (cfws Strict) Strict

-- | Is a character allowed in an atom?
--
-- RFC 5322 §3.2.3
--
-- > atext =   ALPHA / DIGIT /    ; Printable US-ASCII
-- >           "!" / "#" /        ;  characters not including
-- >           "$" / "%" /        ;  specials.  Used for atoms.
-- >           "&" / "'" /
-- >           "*" / "+" /
-- >           "-" / "/" /
-- >           "=" / "?" /
-- >           "^" / "_" /
-- >           "`" / "{" /
-- >           "|" / "}" /
-- >           "~"
--
--  RFC 6532 §3.2
--
-- > atext =/  UTF8-non-ascii
atext :: Mode -> Char -> Bool
atext = \case
  Strict ->
    atext'
  Lenient ->
    \c ->
      atext' c
        || c == ' '
        || c == '\t'
  where
    atext' c =
      isAlphaNum c
        || utf8NonAscii c
        || c == '!'
        || c == '#'
        || c == '$'
        || c == '%'
        || c == '&'
        || c == '\''
        || c == '*'
        || c == '+'
        || c == '-'
        || c == '/'
        || c == '='
        || c == '?'
        || c == '^'
        || c == '_'
        || c == '`'
        || c == '{'
        || c == '|'
        || c == '}'
        || c == '~'

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
quoted :: Mode -> Atto.Parser Atom
quoted mode = quoted' (cfws mode) (cfws mode) mode

-- | General-purpose quoted-string parser.
quoted' ::
  Atto.Parser Comment ->
  Atto.Parser Comment ->
  Mode ->
  Atto.Parser Atom
quoted' lh rh mode = (<?> "quoted content") $ do
  c0 <- optional lh
  _ <- Atto.char '"'
  t <- many (optional (fws mode) *> qcontent <* optional (fws mode))
  _ <- Atto.char '"'
  c1 <- optional rh
  pure (AtomQuoted c0 (mconcat t) c1)
  where
    -- Characters that are allowed in the quotes:
    qcontent = qtext <|> quotedPair mode
    qtext = case mode of
      Strict -> Atto.takeWhile1 isqtext
      Lenient ->
        Atto.takeWhile1
          (\c -> isqtext c || obsNoWsCtl c)
    isqtext c = isasciiqtext (ord c) || utf8NonAscii c
    isasciiqtext n =
      n == 33
        || (n >= 35 && n <= 91)
        || (n >= 93 && n <= 126)

-- | Strict @quoted-string-lh@ from RFC 5322 errata.
--
-- @since 0.1.0.0
quotedLh :: Atto.Parser Atom
quotedLh = quoted' (cfws Strict) (fws Strict) Strict

-- | Parse backslash escapes:
--
-- > quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp
--
-- > obs-qp          =   "\" (%d0 / obs-NO-WS-CTL / LF / CR)
quotedPair :: Mode -> Atto.Parser Text
quotedPair mode = go <?> "quoted char"
  where
    go = one <$> (Atto.char '\\' *> Atto.satisfy allowed)
    allowed c = case mode of
      Strict ->
        vchar c || wsp c
      Lenient ->
        vchar c
          || wsp c
          || obsNoWsCtl c
          || c == '\r'
          || c == '\n'
          || c == '\0'

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
cfws :: Mode -> Atto.Parser Comment
cfws mode =
  (<?> "comment or space") $
    (Comment . mconcat <$> Atto.many1 cfws')
      <|> fws mode
  where
    cfws' = do
      f0 <- fromMaybe Text.empty <$> optional fws'
      c0 <- comment
      f1 <- fromMaybe Text.empty <$> optional fws'
      pure (f0 <> c0 <> f1)
    comment :: Atto.Parser Text
    comment = do
      _ <- Atto.char '('
      ts <- many $ do
        f0 <- fws' <|> pure Text.empty
        cs <- mconcat <$> Atto.many1 ccontent
        pure (f0 <> cs)
      f1 <- fws' <|> pure Text.empty
      _ <- Atto.char ')'
      pure (mconcat ts <> f1)
    ccontent :: Atto.Parser Text
    ccontent = ctext <|> quotedPair mode <|> comment
    ctext :: Atto.Parser Text
    ctext =
      let isctext c = isasciictext (ord c) || utf8NonAscii c
          isasciictext n =
            (n >= 33 && n <= 39)
              || (n >= 42 && n <= 91)
              || (n >= 93 && n <= 126)
       in case mode of
            Strict ->
              Atto.takeWhile1 isctext
            Lenient ->
              Atto.takeWhile1
                (\c -> isctext c || obsNoWsCtl c)
    fws' :: Atto.Parser Text
    fws' = fws mode >>= \case
      Comment t -> pure t
      FoldableSpace t -> pure t

-- | Folding white space.
--
-- > FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
-- >                                        ; Folding white space
-- >
-- > obs-FWS         =   1*WSP *(CRLF 1*WSP)
--
-- @since 0.1.0.0
fws :: Mode -> Atto.Parser Comment
fws = \case
  Strict -> do
    w0 <- fmap (fromMaybe Text.empty) $ optional $ do
      c0 <- many (Atto.satisfy wsp) <&> map one
      cs <- crlf
      pure (mconcat c0 <> cs)
    w1 <- Atto.takeWhile1 wsp
    pure (FoldableSpace (w0 <> w1))
  Lenient -> do
    w0 <- Atto.takeWhile1 wsp
    ws <- many $ do
      c0 <- crlf
      cs <- Atto.takeWhile1 wsp
      pure (c0 <> cs)
    pure (FoldableSpace (w0 <> mconcat ws))
  where
    crlf = Atto.string "\r\n"

-- | Obsolete control characters.
--
-- > obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
-- >                     %d11 /             ;  characters that do not
-- >                     %d12 /             ;  include the carriage
-- >                     %d14-31 /          ;  return, line feed, and
-- >                     %d127              ;  white space characters
--
-- @since 0.1.0.0
obsNoWsCtl :: Char -> Bool
obsNoWsCtl = ord >>> go
  where
    go n =
      (n >= 1 && n <= 8)
        || n == 11
        || n == 12
        || (n >= 14 && n <= 31)
        || n == 127

-- | Whitepace.
--
-- @since 0.1.0.0
wsp :: Char -> Bool
wsp c = c == ' ' || c == '\t'

-- | Visible character.
--
-- @since 0.1.0.0
vchar :: Char -> Bool
vchar c = vchar' (ord c) || utf8NonAscii c
  where
    vchar' n = n >= 0x21 && n <= 0x7e

-- | Is a character in the @UTF8-non-ascii@ class from RFC 6532?
--
-- @since 0.1.0.0
utf8NonAscii :: Char -> Bool
utf8NonAscii c = ord c >= 0xc2 && isPrint c

-- | Parse a domain/address literal.
--
-- > dtext           =   %d33-90 /          ; Printable US-ASCII
-- >                     %d94-126 /         ;  characters not including
-- >                     obs-dtext          ;  "[", "]", or "\"
-- > obs-dtext       =   obs-NO-WS-CTL / quoted-pair
-- @since 0.1.0.0
addressLiteral :: Mode -> Atto.Parser AddressLiteral
addressLiteral mode =
  (<?> "address literal")
    $ Atto.choice
    $ map
      wrap
      [ IpAddress . IP.fromIPv6 <$> (Atto.string "IPv6:" *> IP6.parser),
        TaggedAddress <$> Atto.takeWhile1 (\c -> c /= ':' && isdtext c)
          <*> dtext,
        IpAddress . IP.fromIPv4 <$> IP4.parser,
        AddressLiteral . mconcat <$> many (optional (fws mode) *> dtext)
      ]
  where
    wrap p =
      Atto.char '['
        *> p
        <* optional (fws mode)
        <* Atto.char ']'
    dtext =
      case mode of
        Strict ->
          Atto.takeWhile1 isdtext
        Lenient ->
          mconcat
            <$> Atto.many1
              ( Atto.takeWhile1 (\c -> isdtext c || obsNoWsCtl c)
                  <|> quotedPair mode
              )
    isdtext c = asciidtext (ord c) || utf8NonAscii c
      where
        asciidtext n =
          (n >= 33 && n <= 90)
            || (n >= 94 && n <= 126)
