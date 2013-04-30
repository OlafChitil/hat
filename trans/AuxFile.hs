module AuxFile (readAuxFiles,writeAuxFile) where

import Flags(Flags,sIncludes,sPreludes)
import Environment(Environment,Identifier,AuxiliaryInfo)
import Language.Haskell.Exts.Annotated
  (Module(..),ModuleHead(..),ExportSpecList(..),ExportSpec(..)
  ,ImportDecl(..),ModuleName(..))
import System.FilePath(FilePath,addExtension,pathSeparator,(</>))
import System.IO(stderr,hPutStr)
import System.Exit(exitFailure)
import Control.Exception(catch,IOException)

readAuxFiles :: Flags -> Module l -> IO Environment
readAuxFiles = undefined

-- Read whole content of .hx file of given module, using search paths in flags.
-- Aborts with error if no such .hx file is found.
readAuxFile :: Flags -> ModuleName l -> IO [(Identifier, AuxiliaryInfo)]
readAuxFile flags (ModuleName l moduleStr) = do
  let filePaths = potentialFilePaths flags moduleStr
  (filePath,contents) <- readFirst moduleStr filePaths
  return (map (myRead filePath) . tail . lines $ contents)

-- read function with better error message
myRead :: (Read a) => String -> String -> a
myRead file s =  
  case [x | (x,t) <- reads s, ("","") <- lex t] of
    [x] -> x
    []  -> error ("AuxFile: Cannot parse in .hx file " ++ file ++ " line " ++ s)
    y   -> error ("AuxFile: Ambiguous parse of .hx file " ++ file)

-- For given module name (String) and search paths in flags produce all file paths that .hx file may have.
potentialFilePaths :: Flags -> String -> [FilePath]
potentialFilePaths flags moduleStr = map (</> modPath) paths
  where
  modPath = addExtension (replaceBy '.' pathSeparator moduleStr) "hx"
  paths = sIncludes flags ++ sPreludes flags
  replaceBy :: Char -> Char -> String -> String
  replaceBy c1 c2 = map (\c -> if c==c1 then c2 else c)

-- Given a list of filenames, return filename and its content of first file
-- that was read successfully (intention: other files may not exist)

readFirst :: String -> [FilePath] -> IO (FilePath,String)
readFirst modStr [] = do
  hPutStr stderr ("Fail: .hx file for module " ++ modStr ++ "not found. Give path with -I or -P.") 
  exitFailure
readFirst _ [x] = do 
  finput <- readFile x
  return (x,finput)
readFirst modStr (x:xs) =
  Control.Exception.catch (do finput <- readFile x
                              return (x,finput))
        (\ y -> (y :: Control.Exception.IOException) `seq` readFirst modStr xs)

-- `writeAuxFile' writes out the .hx file given this module's complete
-- parse tree.  The .hx file mentions all exported identifiers, both
-- those defined in this module, and those reexported from imports.
writeAuxFile :: Flags -> FilePath -> Environment -> Module l -> IO ()
writeAuxFile flags filePath env (Module l maybeModuleHead _ _ _) = 
  writeFile filePath ((showString "module " . shows moduleName . showChar '\n' .
                       showLines (listAT (filter (isExported exportSpec)  
  where
  (moduleName,exportSpecs) = case maybeModuleHead of
    Nothing -> let mod = ModuleName l "Main" in (mod, [EVar l (UnQual l "main")])
    Just (ModuleHead _ thisMod _ Nothing) -> (thisMod, [EModuleContents l thisMod])
    Just (ModuleHead _ thisMod _ (Just (ExportSpecList _ list))) -> (thisMod, list)
