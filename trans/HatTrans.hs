-- This `main' function calls all passes of Hat.
-- It parses the .hs source file, creates the .hx file, and
-- then stops immediately after writing a new transformed .hs file.

module Main where

import System.IO(hPutStr,stderr)
import Data.String(unwords)
import Data.List(inits)
import System.Environment(getArgs)
import System.Exit(exitWith,ExitCode(ExitSuccess))
import System.Directory(doesDirectoryExist,createDirectory)
import System.IO.Error(isAlreadyExistsError,ioError)
import qualified Control.Exception(catch)
import Control.Monad(when)
import Flags(processArgs,Flags,sSourceFile,sParse,sFixities,sPrelude,sPreludes,sIncludes,sDbgTrusted
            ,sWrap,sIBound,sShowWidth,sHatAuxFile,sHatTransFile,sSrcDir)
import System.FilePath(FilePath(..),splitDirectories,combine)
import Language.Haskell.Exts(ParseMode(..),Language(..),Extension(..),KnownExtension(..)
         ,ParseResult
         ,fromParseResult,parseFileWithMode
         ,Module(..),ImportDecl(..),ModuleName(..),applyFixities)
import Language.Haskell.Exts.Fixity(Fixity)
import Language.Haskell.Exts.Pretty(prettyPrintStyleMode,Style(..),style,PPHsMode,defaultMode)
import Wrap(wrap)
import Environment(Environment,wiredEnv,globalEnv,prettyEnv,env2Fixities)
import Relation(unionRelations)
import TraceTrans(Tracing(Traced,Trusted),traceTrans)
import AuxFile(readAuxFiles,writeAuxFile)
import SynHelp(getId)
import Paths_hat(getDataDir)

main = do
  args <- getArgs
  let flagsArgs = processArgs args
  preludePath <- getDataDir
  let flags = flagsArgs{sPreludes = preludePath : (sPreludes flagsArgs)}

  let pretty = prettyPrintStyleMode (style{lineLength=sShowWidth flags}) defaultMode

  {- parse source code -}
  let filePath = sSourceFile flags
  let parseMode = ParseMode {parseFilename = filePath
                            ,baseLanguage = Haskell2010
                            ,extensions = [EnableExtension ForeignFunctionInterface
                                          ,EnableExtension NPlusKPatterns]
                            ,ignoreLanguagePragmas = False
                            ,ignoreLinePragmas = True
                            ,fixities = Nothing
                            ,ignoreFunctionArity = False} 
  parseResult <- parseFileWithMode parseMode filePath
  let moduleAST = fromParseResult parseResult
  dumpIntermediate (sParse flags) "Parse" (pretty moduleAST)
    
  {- Ensure we can write our output files. -}
  let hatDir = sSrcDir flags
  dir <- doesDirectoryExist hatDir
  when (not dir) (createDirectoriesRecursively hatDir)

  {- If wrapping rather than transforming, then prepare the parsed AST. -}
  let moduleAST2 = if sWrap flags then wrap (sSourceFile flags) moduleAST else moduleAST
  when (sWrap flags)
       (dumpIntermediate (sParse flags) "Wrap" (pretty moduleAST2))

  {- Add import of Prelude if not done explicitly, and PreludeBasic. -}
  let moduleAST3 = implicitlyImportPreludeBasic flags (implicitlyImportPrelude flags moduleAST2)

  {- Read .hx file of every imported module to produce the complete import environment -}
  importEnv <- readAuxFiles flags moduleAST3

  {- Create global environment of this module. -}
  let env = globalEnv (not (sDbgTrusted flags)) moduleAST3 (unionRelations [importEnv,wiredEnv])

  dumpIntermediate (sIBound flags) "Top-level environment of module" (prettyEnv importEnv)

  {- Write .hx file for current module. -}
  writeAuxFile flags (sHatAuxFile flags) env moduleAST3

  {- Now correct all fixities of expression identifiers. -}
  let fixities = env2Fixities env

  dumpIntermediate (sFixities flags) "Fixities in scope" (unlines . map show $ fixities)

  moduleAST4 <- applyFixities fixities moduleAST3
  
  dumpIntermediate (sParse flags) "After fixity corrections" (pretty moduleAST4)

  {- Actual tracing transformation. -}
  let outputAST = traceTrans (sSourceFile flags)
                    (if sDbgTrusted flags then Trusted else Traced) env moduleAST4

  {- Write result file and finish. -}
  writeFile (sHatTransFile flags) (pretty outputAST)

  putStrLn ("Wrote " ++ sHatTransFile flags)
  exitWith (ExitSuccess)

createDirectoriesRecursively :: FilePath -> IO () 
createDirectoriesRecursively path = do
  putStrLn ("Creating directories "++unwords paths)
  mapM_ safeCreate paths
  where 
  paths = recursiveDirectories path
  safeCreate path = Control.Exception.catch (createDirectory path)
                      (\e-> if isAlreadyExistsError e then return () else ioError e)

recursiveDirectories :: FilePath -> [FilePath]
recursiveDirectories path = map (foldr1 combine) . tail . inits . splitDirectories $ path

-- Add implicit Prelude, if it is not imported explicitly and we 
-- are not currently transforming part of the Prelude.
implicitlyImportPrelude :: Flags -> Module l -> Module l
implicitlyImportPrelude flags
  (Module l maybeHead pragmas importDecls decls) =
  Module l maybeHead pragmas importDecls' decls
  where
  importDecls' = if sPrelude flags ||  "Prelude" `elem` map (getId . importModule) importDecls
                   then importDecls
                   else ImportDecl{importAnn = l, importModule = ModuleName l "Prelude"
                                  ,importQualified = False, importSrc = False, importSafe = False
                                  ,importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} : importDecls

-- Add import PreludeBasic if current module is not part of the Prelude.
-- Module PreludeBasic defines identifiers introduced by the transformation or deriving
-- and *not* exported by module Prelude.
implicitlyImportPreludeBasic :: Flags -> Module l -> Module l
implicitlyImportPreludeBasic flags 
  (Module l maybeHead pragmas importDecls decls) =
  Module l maybeHead pragmas importDecls' decls
  where
  importDecls' = if sPrelude flags
                   then importDecls
                   else ImportDecl{importAnn = l, importModule = ModuleName l "PreludeBasic"
                                  ,importQualified = True, importSrc = False, importSafe = False
                                  ,importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} : 
                        ImportDecl{importAnn = l, importModule = ModuleName l "PreludeBuiltinTypes"
                                  ,importQualified = True, importSrc = False, importSafe = False
                                  ,importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} : importDecls

{- If first argument is True, then print second and third with formatting -}
dumpIntermediate :: Bool -> String -> String -> IO ()
dumpIntermediate flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
    else return ()


{- End Module Main ----------------------------------------------------------}
