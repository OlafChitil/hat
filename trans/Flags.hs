{- ---------------------------------------------------------------------------
Flags are all the choices and information given to the compiler in the 
argument list. Here a data type Flags is defined for holding this information,
a function processArgs to obtain a value of type Flags from the argument list,
and a simple function pF for printing information demanded by a flag.
-}
module Flags
  (Flags,processArgs,pF
  ,sUnix
  ,sUnlit
  ,sSourceFile
  ,sUnderscore
  ,sLex
  ,sParse
  ,sTraceFns
  ,sPrelude
  ,sPreludes
  ,sIncludes
  ,sDbgTrusted
  ,sWrap
  ,sRealFile
  ,sIBound
  ,sShowWidth
  ,sShowIndent
  ,sShowQualified
  ,sHatAuxFile
  ,sHatTransFile
  ,sHatFileBase
  ,sSrcDir
  ) where

import System.IO
import OsOnly(fixRootDir,fixTypeFile,fixObjectFile
             ,fixHatAuxFile,fixHatTransFile,fixHatTransDir,fixHatFileBase)
import Data.List(isPrefixOf,isSuffixOf)
import Data.Char(isDigit)


data Flags = FF 
  {sRealFile   :: String
  ,sSourceFile :: String
  ,sHatAuxFile   :: String
  ,sHatTransFile :: String
  ,sHatFileBase  :: String
  ,sIncludes   :: [String]
  ,sPreludes   :: [String]
  ,sSrcDir     :: String

--v Flags to control compilation
  ,sUnix       :: Bool	-- either unix or RiscOS
  ,sUnlit      :: Bool	-- unliterate the source code
  ,sPrelude    :: Bool	-- keep prelude defns in interface file

--v Flags to control compilation for tracing
  ,sDbgTrusted :: Bool	-- trust this module
  ,sWrap       :: Bool	-- wrap original defns rather than transforming

--v Flags for machine architecture / configuration
  ,sUnderscore :: Bool	-- force H'98 underscores

--v debugging flags - show program / import tables (after each compiler phase)
  ,sLex        :: Bool	-- input	after lexing
  ,sParse      :: Bool	-- ast		after parsing
  ,sTraceFns   :: Bool	-- ast		after tracing transform (fns)
  ,sIBound     :: Bool	-- aux tree     after ast annotation

--v pretty-printing flags
  ,sShowWidth  :: Int   -- width for showing intermediate program
  ,sShowIndent :: Int   -- indentation for nesting shown intermediate program
  ,sShowQualified :: Bool -- show qualified ids as far as possible
  }
  deriving Show



{- If first argument is True, then print second and third with formatting -}
pF :: Bool -> [Char] -> [Char] -> IO ()
pF flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
    else return ()

{- ---------------------------------------------------------------------------
All the following functions obtain information from the argument list of the
compiler to set flags appropriately.
-}

{-
The main function for processing the argument list.
Aborts with error, if the required filenames are not in argument list.
(But no further error checking)
-}

processArgs :: [String] -> Flags

processArgs xs = flags
 where
 (rootdir,filename) = fixRootDir isUnix sourcefile
 isUnix = sUnix flags

 (realfile,sourcefile) =
   case getFiles xs of
     [sourcefile] -> (sourcefile,sourcefile)
     [realfile,sourcefile] -> (realfile,sourcefile)
     _ -> error ("\nUsage: hat-trans file.[l]hs\n\ 
\       hat-trans tmpfile.[l]hs origfile.[l]hs\n")  

 flags = FF
  { sRealFile   = realfile	-- filename to open
  , sSourceFile = sourcefile	-- original name before any preprocessor
  , sHatAuxFile = fixHatAuxFile isUnix rootdir filename
  , sHatTransFile = fixHatTransFile isUnix rootdir filename
  , sHatFileBase  = fixHatFileBase isUnix rootdir filename
  , sIncludes = getIncludes xs++[rootdir]
  , sPreludes = getPreludes xs
  , sSrcDir   = fixHatTransDir isUnix rootdir

  , sUnix = fElem True  "unix" xs          	
  -- ^ Use unix file names
  , sUnlit = fElem (".lhs" `isSuffixOf` realfile) "unlit" xs         	
  -- ^ Unliterate the source code
  , sPrelude = fElem False "prelude" xs		
  -- Keep prelude definitions in interface file

  , sDbgTrusted = fElem False "trusted" xs    -- "trusted" module (don't trace)
  , sWrap       = fElem False "wrap" xs       -- "wrapped" module (don't trace)

  , sUnderscore = fElem True "underscore" xs 
  -- ^ Enable H'98 underscore-is-lower-case

  , sLex = fElem False "lex" xs         -- show lexical input
  , sParse  = fElem False "parse" xs    -- show syntax tree  after  parser
  , sTraceFns = fElem False "tracefns" xs  -- ast after transforming functions
  , sIBound = fElem False "ibound" xs   -- aux tree after ast annotation

  , sShowWidth = cardFlag 80 "showwidth=" xs  -- set width for showing 
                                              -- intermediate program
  , sShowIndent = cardFlag 2 "showindent=" xs -- set indentation for nesting
  , sShowQualified = fElem True "showqualified" xs  
  -- ^ show qualified ids as far as possible
  }
  
  
{- obtain list of filenames from argument list -}
getFiles :: [String] -> [String]
getFiles = filter (\xs -> case xs of ('-':_) -> False ; _ -> True)


{- obtain list of include paths from argument list -}
getIncludes :: [String] -> [String]
getIncludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'I':_) -> True  
                                        ('-':'i':_) -> True  
                                        _           -> False)

{- obtain list of prelude paths from argument list -}
getPreludes :: [String] -> [String]
getPreludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'P':_) -> True ; _ -> False)


{-
Returns if given option is set or not in argument list.
If it is neither set nor unset, then default value (first arg.) is returned.
-}
fElem :: Bool -> [Char] -> [String] -> Bool
fElem def f flags = if ('-':f) `elem` flags then True
                    else if ('-':'n':'o':f) `elem` flags then False
                    else def


{-
Returns the value of an option with a numerical (cardinal) value.
If the option is not given, then the default value (first arg.) is returned.
Ignores syntactically incorrect options.
-}
cardFlag :: Int -> [Char] -> [String] -> Int
cardFlag def f flags = if null settings then def else read (last settings)
  where
  settings = filter (all isDigit) . map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags



{-
Returns the value of a "-something=" option with a string value.
If the option is not given, then the default value (first arg.) is returned.
-}
stringFlag :: String -> String -> [String] -> String
stringFlag def f flags = if null settings then def else last settings
  where
  settings = map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags

