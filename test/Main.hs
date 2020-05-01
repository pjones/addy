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
module Main
  ( main,
  )
where

import qualified ParserTest
import qualified RenderTest
import Test.Tasty

-- | Main.
main :: IO ()
main =
  defaultMain $
    testGroup
      "Email"
      [ ParserTest.test,
        RenderTest.test
      ]
