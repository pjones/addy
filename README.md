Addy: a modern library for working with email addresses
=======================================================

[![addy](https://circleci.com/gh/pjones/addy.svg?style=shield)](https://app.circleci.com/github/pjones/addy/pipelines)
[![Hackage](https://img.shields.io/hackage/v/addy?logo=haskell)](https://hackage.haskell.org/package/addy)
[![GitHub](https://img.shields.io/github/license/pjones/addy)](https://github.com/pjones/addy/blob/master/LICENSE)

A full-featured library for parsing, validating, and rendering email
addresses.

Decoding
--------

```haskell
Addy.decode "example@example.com"
-- Right (EmailAddr "example@example.com")

Addy.decode "我買@屋企.香港"
-- Right (EmailAddr "\25105\36023@\23627\20225.\39321\28207")

Addy.decode "Mary Smith <mary@example.net> (hi there!)"
-- Right (EmailAddr "Mary Smith <mary@example.net> (hi there!)")

Addy.decode "example@[127.0.0.1]"
-- Right (EmailAddr "example@[127.0.0.1]")
```

Encoding
--------

```haskell
Addy.encode address
-- "example@example.com"

Addy.decode "Mary Smith <mary@example.net> (hi there!)"
  & second Addy.encodeFull
-- Right "Mary Smith <mary@example.net> (hi there!)"
```
