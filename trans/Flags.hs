{- ---------------------------------------------------------------------------
Flags are all the choices and information given to the compiler in the 
argument list. Here a data type Flags is defined for holding this information,
a function processArgs to obtain a value of type Flags from the argument list,
and a simple function pF for printing information demanded by a flag.
-}
module Flags
  (Flags,processArgs
  ,sSourceFile
  ,sParse
  ,sPrelude
  ,sPreludes
  ,sIncludes
  ,sDbgTrusted
  ,sWrap
  ,sIBound
  ,sShowWidth
  ,sHatAuxFile
  ,sHatTransFile
  ,sSrcDir
  ,sFixities
  ) where

import System.Console.GetOpt(getOpt,usageInfo,ArgOrder(..),OptDescr(..),ArgDescr(..))
import System.FilePath(FilePath(..),splitFileName,replaceExtension,takeExtension,(</>))
import Data.Char(isDigit)

data Flags = FF 
  {sSourceFile :: String
  ,sHatAuxFile   :: String
  ,sHatTransFile :: String
  ,sIncludes   :: [String]
  ,sPreludes   :: [String]
  ,sSrcDir     :: String

--v Flags to control compilation
  ,sPrelude    :: Bool	-- keep prelude defns in interface file

--v Flags to control compilation for tracing
  ,sDbgTrusted :: Bool	-- trust this module
  ,sWrap       :: Bool	-- wrap original defns rather than transforming

--v debugging flags - show program / import tables (after each compiler phase)
  ,sParse      :: Bool	-- ast		after parsing
  ,sIBound     :: Bool	-- environment
  ,sFixities   :: Bool  -- fixity declarations in top-level scope

--v pretty-printing flags
  ,sShowWidth  :: Int   -- width for showing intermediate program
  }
  deriving Show

options :: [OptDescr (Flags -> Flags)]
options =
  [Option ['i','I'] []
     (ReqArg (\p flags -> flags{sIncludes = sIncludes flags ++ [p]}) "DIR")
     "Search path for included (imported) modules."
  ,Option ['P'] []
     (ReqArg (\p flags -> flags{sPreludes = sPreludes flags ++ [p]}) "DIR")
     "Search path for prelude."
  ,Option [] ["prelude"] 
     (NoArg (\flags -> flags{sPrelude = True}))
     "Input module is part of the prelude." -- Needs some special handling of imports.
  ,Option [] ["hierarchical"]
     (NoArg (\flags -> let (rootdir,filename) = splitFileName (sSourceFile flags)
                       in flags{sSrcDir = "Hat" </> rootdir
                               ,sHatTransFile = "Hat" </> rootdir </> replaceExtension filename "hs"}))
     "Module belongs in hierarchical library." -- So output in directory Hat/hierachy...
  ,Option [] ["trusted"]
     (NoArg (\flags -> flags{sDbgTrusted = True}))
     "Input module shall be trusted, not traced."
  ,Option [] ["wrap"]
     (NoArg (\flags -> flags{sWrap = True}))
     "Wrap definitions of input module instead of transforming."
  ,Option [] ["parse"]
     (NoArg (\flags -> flags{sParse = True}))
     "Show syntax tree after parsing."
  ,Option [] ["env"]
     (NoArg (\flags -> flags{sIBound = True}))
     "Show environment that is written to .aux file."
  ,Option [] ["fixities"]
     (NoArg (\flags -> flags{sFixities = True}))
     "Show all fixity declarations in scope."
  ,Option [] ["width"]
     (ReqArg (\w flags -> flags{sShowWidth = if all isDigit w then read w else 80}) "NAT")
     "Width for showing intermediate programs (syntax trees)."
  ]

{-
The main function for processing the argument list.
Aborts with error, if the required filenames are not in argument list.
(But no further error checking)
-}

processArgs :: [String] -> Flags
processArgs argv = 
  flags {sIncludes = sIncludes flags ++ [fst (splitFileName (sSourceFile flags))]} 
     -- add rootdir at end
  where
  flags = case getOpt Permute options argv of
    (o,[f],[]) | takeExtension f `elem` [".hs",".lhs"] -> foldl (flip id) (defaultFlags f) o
    (_,_,errs) -> error (concat errs ++ usageInfo header options)
  header = "Usage: hat-trans [OPTION...] file.[l]hs\n"

defaultFlags :: String -> Flags
defaultFlags sourcefile = flags
  where
  (rootdir,filename) = splitFileName sourcefile
  flags = FF
    {sSourceFile = sourcefile	-- original name 
    ,sHatAuxFile = rootdir </> replaceExtension filename "hx"
    ,sHatTransFile = rootdir </> "Hat" </> replaceExtension filename "hs"
    ,sIncludes = []
    ,sPreludes = []
    ,sSrcDir   = rootdir </> "Hat"  -- directory for transformed sources
    ,sPrelude = False		
    ,sDbgTrusted = False
    ,sWrap       = False
    ,sParse  = False
    ,sIBound = False
    ,sFixities = False
    ,sShowWidth = 80 
    }
