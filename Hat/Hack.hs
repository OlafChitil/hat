{-# LANGUAGE CPP #-}
module Hat.Hack
  (
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )
#else
    []((:),[])
#endif
  ) where

#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )
#else
import Prelude
#endif
