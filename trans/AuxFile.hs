module AuxFile (readAuxFiles,writeAuxFile) where

import Flags(Flags)
import Environment(Environment)
import Language.Haskell.Exts.Annotated(Module(..),ImportDecl(..),ModuleName(..))

readAuxFiles :: Flags -> Module l -> IO Environment
readAuxFiles = undefined

writeAuxFile :: Flags -> FilePath -> Environment -> Module l -> IO ()
writeAuxFile = undefined