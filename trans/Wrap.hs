module Wrap (wrap) where

import Language.Haskell.Exts.Annotated
import System.FilePath(FilePath)

wrap :: FilePath -> Module SrcSpanInfo -> Module SrcSpanInfo
wrap = undefined