{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Text.Internal.Exception
-- Copyright   : (c) 2026 text maintainers
-- License     : BSD-style
-- Maintainer  : lysxia@gmail.com
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Compatibility shims for re-throwing exceptions.
-- GHC 9.12 implements this API, and we fake it for older GHCs: rethrow is just throw.
module Data.Text.Internal.Exception
  ( ExceptionWithContext(..)
  , ExceptionContext
  , catchNoPropagate
  , rethrowIO
  , someExceptionContext
  , rethrow
  ) where

#if __GLASGOW_HASKELL__ >= 912
import Control.Exception
import Control.Exception.Context (ExceptionContext)

rethrow :: Exception e => ExceptionWithContext e -> a
rethrow = throw . NoBacktrace
#else
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
import GHC.Stack (HasCallStack, withFrozenCallStack)

type ExceptionContext = ()
data ExceptionWithContext e = ExceptionWithContext ExceptionContext e

catchNoPropagate :: Exception e => IO a -> (ExceptionWithContext e -> IO a) -> IO a
catchNoPropagate io h = E.catch io (h . ExceptionWithContext ())

rethrowIO :: Exception e => ExceptionWithContext e -> IO a
rethrowIO (ExceptionWithContext () e) = withFrozenCallStack (E.throwIO e)

someExceptionContext :: SomeException -> ExceptionContext
someExceptionContext _ = ()

rethrow :: (Exception e, HasCallStack) => ExceptionWithContext e -> a
rethrow (ExceptionWithContext () e) = withFrozenCallStack (E.throw e)
#endif
