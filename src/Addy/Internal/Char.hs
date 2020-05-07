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
-- Internal functions representing character classes in email
-- addresses.
--
-- Obsolete characters are only supported in
-- 'Addy.Internal.Parser.Lenient' mode and are filtered out after
-- parsing.
module Addy.Internal.Char
  ( utf8NonAscii,
    obsNoWsCtl,
    wsp,
    vchar,
    atext,
    dtext,
    ctext,
    ctextObs,
    qtext,
    qtextObs,
    quotedPair,
    quotedPairObs,
  )
where

import Data.Char

-- | Is a character in the @UTF8-non-ascii@ class from RFC 6532?
--
-- @since 0.1.0.0
utf8NonAscii :: Char -> Bool
utf8NonAscii c = ord c >= 0xc2 && isPrint c

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

-- | RFC 5234: Visible character.
--
-- > VCHAR          =  %x21-7E
-- >                        ; visible (printing) characters
--
--  RFC 6532 §3.2
--
-- > VCHAR   =/  UTF8-non-ascii
--
-- @since 0.1.0.0
vchar :: Char -> Bool
vchar c = vchar' (ord c) || utf8NonAscii c
  where
    vchar' n = n >= 0x21 && n <= 0x7e

-- | RFC 5322 §3.2.3
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
--
-- @since 0.1.0.0
atext :: Char -> Bool
atext c =
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

-- | RFC 5322 @dtext@.
--
-- > dtext           =   %d33-90 /          ; Printable US-ASCII
-- >                     %d94-126 /         ;  characters not including
-- >                     obs-dtext          ;  "[", "]", or "\"
-- > obs-dtext       =   obs-NO-WS-CTL / quoted-pair
--
--  RFC 6532 §3.2
--
-- > dtext   =/  UTF8-non-ascii
-- @since 0.1.0.0
dtext :: Char -> Bool
dtext c = asciidtext (ord c) || utf8NonAscii c
  where
    asciidtext n =
      (n >= 33 && n <= 90)
        || (n >= 94 && n <= 126)

-- | RFC 5322 @ctext@.
--
-- > ctext           =   %d33-39 /          ; Printable US-ASCII
-- >                     %d42-91 /          ;  characters not including
-- >                     %d93-126 /         ;  "(", ")", or "\"
-- >                     obs-ctext
-- >
-- > obs-ctext       =   obs-NO-WS-CTL
--
-- RFC 6532 §3.2
--
-- > ctext   =/  UTF8-non-ascii
--
-- @since 0.1.0.0
ctext :: Char -> Bool
ctext c = asciictext (ord c) || utf8NonAscii c
  where
    asciictext n =
      (n >= 33 && n <= 39)
        || (n >= 42 && n <= 91)
        || (n >= 93 && n <= 126)

-- | Obsolete @ctext@.
--
-- > obs-ctext = obs-NO-WS-CTL
--
-- @since 0.1.0.0
ctextObs :: Char -> Bool
ctextObs = obsNoWsCtl

-- | Characters that can appear in a quoted string.
--
-- RFC 5322 §3.2.4:
--
-- > qtext =   %d33 /             ; Printable US-ASCII
-- >           %d35-91 /          ;  characters not including
-- >           %d93-126 /         ;  "\" or the quote character
-- >           obs-qtext
--
-- RFC 6532 §3.2:
--
-- > qtext =/ UTF8-non-ascii
-- @since 0.1.0.0
qtext :: Char -> Bool
qtext c = asciiqtext (ord c) || utf8NonAscii c
  where
    asciiqtext n =
      n == 33
        || (n >= 35 && n <= 91)
        || (n >= 93 && n <= 126)

-- | Obsolete @qtext@.
--
-- > obs-qtext = obs-NO-WS-CTL
--
-- @since 0.1.0.0
qtextObs :: Char -> Bool
qtextObs = obsNoWsCtl

-- | Characters that can follow a backslash.
--
-- > quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp
--
-- @since 0.1.0.0
quotedPair :: Char -> Bool
quotedPair c = vchar c || wsp c

-- | Obsolete characters that can be escaped with a backslash.
--
-- > obs-qp          =   "\" (%d0 / obs-NO-WS-CTL / LF / CR)
--
-- @since 0.1.0.0
quotedPairObs :: Char -> Bool
quotedPairObs c =
  obsNoWsCtl c
    || c == '\r'
    || c == '\n'
    || c == '\0'
