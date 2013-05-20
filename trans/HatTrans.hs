{- ---------------------------------------------------------------------------
This `main' function calls all passes of Hat.
It parses the .hs source file, creates the .hx file, and
then stops immediately after writing a new transformed .hs file.
-} 
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
import Flags(processArgs,Flags,sSourceFile,sParse,sPrelude,sPreludes,sIncludes,sDbgTrusted
            ,sWrap,sIBound,sShowWidth,sHatAuxFile,sHatTransFile,sSrcDir)
import System.FilePath(FilePath(..),splitDirectories,combine)
import Language.Haskell.Exts.Annotated(ParseMode(..),ParseResult,fromParseResult,parseFileWithMode
                                      ,Module(..),ImportDecl(..),ModuleName(..),Extension(..))
import Language.Haskell.Exts.Fixity(Fixity,preludeFixities)
import Language.Haskell.Exts.Pretty(prettyPrintStyleMode,Style(..),style,PPHsMode,defaultMode)
import Wrap(wrap)
import Environment(Environment,globalEnv,prettyEnv,exports)
import TraceTrans(Tracing(Traced,Trusted),traceTrans)
import AuxFile(readAuxFiles,writeAuxFile)

main = do
  args <- getArgs
  let flags = processArgs args

  let pretty = prettyPrintStyleMode (style{lineLength=sShowWidth flags}) defaultMode

  {- parse source code -}
  let filePath = sSourceFile flags
  let parseMode = ParseMode {parseFilename = filePath
                            ,extensions = [ForeignFunctionInterface]
                            ,ignoreLanguagePragmas = False
                            ,ignoreLinePragmas = True
                            ,fixities = Just preludeFixities} 
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

  {- Add import of Prelude if not done explicitly. -}
  let moduleAST3 = implicitlyImportPrelude flags moduleAST2
  dumpIntermediate (sParse flags) "post-Prelude" (pretty moduleAST3)

  {- Read .aux file of every imported module to produce the complete import environment -}
  importEnv <- readAuxFiles flags moduleAST3

  {- Create global environment of this module. -}
  let env = globalEnv (not (sDbgTrusted flags)) moduleAST3 importEnv

  {- Write .hx file for current module. -}
  writeAuxFile flags (sHatAuxFile flags) env moduleAST3

  dumpIntermediate (sIBound flags) "Top-level environment of module" (prettyEnv env)
  
{-
  {- Actual tracing transformation. -}
  let outputAST = implicitlyImportPreludeBasic flags 
                    (traceTrans (sSourceFile flags) (if sDbgTrusted flags then Trusted else Traced) env moduleAST3)

  {- Write result file and finish. -}
  writeFile (sHatTransFile flags) (pretty outputAST)
-}
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
  importDecls' = if sPrelude flags || 
                      (any (== "Prelude") . map ((\(ModuleName _ str) -> str) . importModule) $ importDecls)
                   then importDecls
                   else ImportDecl{importAnn = l, importModule = ModuleName l "Prelude"
                                  ,importQualified = False, importSrc = False
                                  ,importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} : importDecls

-- Add import Hat.PreludeBasic if current module is not part of the Prelude
-- Module Hat.PreludeBasic defines identifiers introduced by the transformation or deriving
implicitlyImportPreludeBasic :: Flags -> Module l -> Module l
implicitlyImportPreludeBasic flags 
  (Module l maybeHead pragmas importDecls decls) =
  Module l maybeHead pragmas importDecls' decls
  where
  importDecls' = if sPrelude flags 
                   then importDecls
                   else ImportDecl{importAnn = l, importModule = ModuleName l "Hat.PreludeBasic"
                                  ,importQualified = False, importSrc = False
                                  ,importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} : importDecls

{- If first argument is True, then print second and third with formatting -}
dumpIntermediate :: Bool -> String -> String -> IO ()
dumpIntermediate flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
    else return ()


{- End Module Main ----------------------------------------------------------}
