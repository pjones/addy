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
  ( Mode (..),
    parse,
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
    utf8NonAscii,
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
import Relude.Extra.Lens

-- |
data Mode
  = Strict
  | Lenient
  deriving (Eq, Show)

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
            <|> (LocalAtom <$> quotedLh <* thenAt)
        Lenient ->
          -- Obsolete comes before quoted since the obsolete syntax allows
          -- multiple quoted strings separated by dots.
          (LocalAtom <$> dotAtom mode <* thenAt)
            <|> obsLocalPart <* thenAt
            <|> (LocalAtom <$> quoted mode <* thenAt)
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
      DomainAtom atom -> traverse_ (traverse_ checkLabel . (^. dots)) atom
      DomainLiteral {} -> pass
      DomainObsolete atoms -> traverse_ (traverse_ checkLabel) atoms
    checkLabel label =
      if "-" `Text.isPrefixOf` (label ^. text)
        || "-" `Text.isSuffixOf` (label ^. text)
        then fail "must not start or end with a hypen"
        else pass
    -- > domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
    domainLiteral lh = do
      _ <- optional lh -- Don't capture because it's obsolete.
      t <- addressLiteral mode
      cs <- optional (cfws mode)
      pure (DomainLiteral t cs)
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
    <*> atext mode
    <*> optional (cfws mode)

-- | Parse an unquoted atom that is allowed to contain periods.
--
-- @since 0.1.0.0
dotAtom' ::
  Atto.Parser Comments ->
  Atto.Parser Comments ->
  Mode ->
  Atto.Parser Atom
dotAtom' lh rh mode = do
  c0 <- optional lh
  t0 <- atext mode
  ts <- many (Atto.char '.' *> atext mode)
  c1 <- optional rh
  pure (Atom c0 (t0 & dots .~ (t0 : ts)) c1)

-- | RFC 5322 @dot-atom@.
dotAtom :: Mode -> Atto.Parser Atom
dotAtom mode = dotAtom' (cfws mode) (cfws mode) mode

-- | Strict @dot-atom-lh@ from RFC 5322 errata.
--
-- > dot-atom-lh = [CFWS] dot-atom-text [FWS]
--
-- @since 0.1.0.0
dotAtomLh :: Atto.Parser Atom
dotAtomLh =
  dotAtom'
    (cfws Strict)
    (FoldingSpace <$> fws Strict)
    Strict

-- | Strict @dot-atom-rh@ from RFC 5322 errata.
--
-- > dot-atom-rh = [FWS] dot-atom-text [CFWS]
--
-- @since 0.1.0.0
dotAtomRh :: Atto.Parser Atom
dotAtomRh =
  dotAtom'
    (FoldingSpace <$> fws Strict)
    (cfws Strict)
    Strict

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
atext :: Mode -> Atto.Parser Content
atext = \case
  Strict ->
    Atto.takeWhile1 atext'
      <&> CleanText
  Lenient ->
    Atto.takeWhile1 obs
      <&> ObsText
  where
    obs c =
      atext' c
        || c == ' '
        || c == '\t'
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
  Atto.Parser Comments ->
  Atto.Parser Comments ->
  Mode ->
  Atto.Parser Atom
quoted' lh rh mode = (<?> "quoted content") $ do
  c0 <- optional lh
  _ <- Atto.char '"'
  t <- many ((<>) <$> fws' <*> qcontent)
  w <- fws'
  _ <- Atto.char '"'
  c1 <- optional rh
  pure (AtomQuoted c0 (mconcat t <> w) c1)
  where
    -- Characters that are allowed in the quotes:
    qcontent = qtext <|> quotedPair mode
    qtext = case mode of
      Strict -> Atto.takeWhile1 isqtext <&> CleanText
      Lenient ->
        Atto.takeWhile1
          (\c -> isqtext c || obsNoWsCtl c)
          <&> ObsText
    isqtext c = isasciiqtext (ord c) || utf8NonAscii c
    isasciiqtext n =
      n == 33
        || (n >= 35 && n <= 91)
        || (n >= 93 && n <= 126)
    fws' = fws mode <|> pure mempty

-- | Strict @quoted-string-lh@ from RFC 5322 errata.
--
-- @since 0.1.0.0
quotedLh :: Atto.Parser Atom
quotedLh =
  quoted'
    (cfws Strict)
    (FoldingSpace <$> fws Strict)
    Strict

-- | Parse backslash escapes:
--
-- > quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp
--
-- > obs-qp          =   "\" (%d0 / obs-NO-WS-CTL / LF / CR)
quotedPair :: Mode -> Atto.Parser Content
quotedPair mode = go <?> "quoted char"
  where
    go = Atto.char '\\' *> allowed
    allowed = case mode of
      Strict ->
        Atto.satisfy
          ( \c ->
              vchar c || wsp c
          )
          <&> (one >>> CleanText)
      Lenient ->
        Atto.satisfy
          ( \c ->
              vchar c
                || wsp c
                || obsNoWsCtl c
                || c == '\r'
                || c == '\n'
                || c == '\0'
          )
          <&> (one >>> ObsText)

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
cfws :: Mode -> Atto.Parser Comments
cfws mode =
  (<?> "comment or space")
    (cfws' <|> (FoldingSpace <$> fws mode))
  where
    cfws' = do
      c : cs <- Atto.many1 ((,) <$> optional fws' <*> comment)
      fs <- optional fws'
      pure (Comments (c :| cs) fs)
    comment :: Atto.Parser Comment
    comment = do
      _ <- Atto.char '('
      ts <-
        many
          ( (<>) <$> (fws' <|> pure mempty)
              <*> (mconcat <$> Atto.many1 ccontent)
          )
      fs <- optional fws'
      _ <- Atto.char ')'
      pure (Comment (mconcat ts) fs)
    ccontent :: Atto.Parser Content
    ccontent =
      ctext
        <|> quotedPair mode
        <|> ( do
                Comment t0 t1 <- comment
                pure (t0 <> fromMaybe mempty t1)
            )
    ctext :: Atto.Parser Content
    ctext =
      let isctext c = isasciictext (ord c) || utf8NonAscii c
          isasciictext n =
            (n >= 33 && n <= 39)
              || (n >= 42 && n <= 91)
              || (n >= 93 && n <= 126)
       in case mode of
            Strict ->
              Atto.takeWhile1 isctext <&> CleanText
            Lenient ->
              Atto.takeWhile1
                (\c -> isctext c || obsNoWsCtl c)
                <&> ObsText
    fws' :: Atto.Parser Content
    fws' = fws mode

-- | Folding white space.
--
-- > FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
-- >                                        ; Folding white space
-- >
-- > obs-FWS         =   1*WSP *(CRLF 1*WSP)
--
-- @since 0.1.0.0
fws :: Mode -> Atto.Parser Content
fws = \case
  Strict -> do
    w0 <- (Atto.takeWhile wsp <* crlf) <|> pure Text.empty
    w1 <- Atto.takeWhile1 wsp
    pure $ CleanText (w0 <> w1)
  Lenient -> do
    w0 <- Atto.takeWhile1 wsp
    ws <- many (crlf *> Atto.takeWhile1 wsp)
    -- Text here is clean because we strip out the control characters.
    pure $ CleanText (w0 <> mconcat ws)
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
        TaggedAddress <$> tag <*> (Atto.char ':' *> dtext),
        IpAddress . IP.fromIPv4 <$> IP4.parser,
        AddressLiteral . mconcat <$> many ((<>) <$> fws' <*> dtext)
      ]
  where
    wrap p =
      Atto.char '['
        *> p
        <* optional (fws mode)
        <* Atto.char ']'
    tag = Atto.takeWhile1 (\c -> c /= ':' && isdtext c) <&> CleanText
    dtext =
      case mode of
        Strict ->
          Atto.takeWhile1 isdtext
            <&> CleanText
        Lenient ->
          mconcat
            <$> Atto.many1
              ( ( Atto.takeWhile1 (\c -> isdtext c || obsNoWsCtl c)
                    <&> ObsText
                )
                  <|> quotedPair mode
              )
    isdtext c = asciidtext (ord c) || utf8NonAscii c
      where
        asciidtext n =
          (n >= 33 && n <= 90)
            || (n >= 94 && n <= 126)
    fws' = fws mode <|> pure mempty
