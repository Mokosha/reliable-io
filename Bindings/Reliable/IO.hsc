{-|
  Module      : Bindings.Reliable.IO
  Description : Low-level bindings to the reliable.io library.
  Copyright   : (c) Pavel Krajcevski, 2020
  License     : BSD-3
  Maintainer  : krajcevski@gmail.com
  Stability   : experimental
  Portability : Portable

  This module contains the low-level bindings that represent the direct
  interface between Haskell and the C library
  <https://github.com/networkprotocol/reliable.io reliable.io>.

  The bindings here are meant for advanced usage, as they are not particularly
  idiomatic Haskell, and largely represent the types that we get from the
  <https://hackage.haskell.org/package/bindings-DSL bindings-DSL> library. For
  high level bindings (recommended), please refer to the "Reliable.IO" module.
-}

{-# LANGUAGE NoImplicitPrelude  #-}
--------------------------------------------------------------------------------

#include <reliable.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Reliable.IO where

import Foreign.C.Types        (CInt(..))
import Foreign.Ptr            (FunPtr)
import Prelude                (IO, Num)

--------------------------------------------------------------------------------

#num RELIABLE_OK
#num RELIABLE_ERROR

#ccall reliable_init, IO CInt
#ccall reliable_term, IO ()

-- | For testing only.
#ccall reliable_test, IO ()
