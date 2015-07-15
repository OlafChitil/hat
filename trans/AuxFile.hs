module AuxFile (readAuxFiles,writeAuxFile) where

import Flags(Flags,sIncludes,sPreludes,sDbgTrusted)
import Environment(Environment,Entity,HxEntity,exports,imports,hxEnvironmentToList,listToHxEnvironment)
import Relation(unionRelations)
import Language.Haskell.Exts.Annotated
  (Module(..),ModuleHead(..),ExportSpecList(..),ExportSpec(..)
  ,ImportDecl(..),ModuleName(..),QName(..),SrcSpanInfo)
import SynHelp (mkQName,Id(getId),getModuleNameFromModule)
import System.FilePath (FilePath,addExtension,pathSeparator,(</>))
import System.IO (stderr,hPutStr)
import System.Exit (exitFailure)
import qualified Control.Exception(catch,IOException)
import Data.List (isPrefixOf)

-- Create environment for all imports.
-- Exception if hx-file of an imported module is not found.
readAuxFiles :: Flags -> Module SrcSpanInfo -> IO Environment
readAuxFiles flags mod@(Module l maybeModuleHead _ importDecls decls) = do
  importEnvs <- mapM (importEnv flags) (filter notNotHat importDecls)
  return (unionRelations importEnvs)

notNotHat :: ImportDecl l -> Bool
notNotHat importDecl = not ("NotHat" `isPrefixOf` getId (importModule importDecl))

importEnv :: Flags -> ImportDecl SrcSpanInfo -> IO Environment
importEnv flags importDecl = do
  entities <- readAuxFile flags (importModule importDecl)
  return (imports (listToHxEnvironment entities) importDecl)


-- Read whole content of .hx file of given module, using search paths in flags.
-- Aborts with error if no such .hx file is found.
readAuxFile :: Flags -> ModuleName l -> IO [HxEntity]
readAuxFile flags (ModuleName l moduleStr) = do
  let filePaths = potentialFilePaths flags moduleStr
  (filePath,contents) <- readFirst moduleStr filePaths filePaths
  return (map (myRead filePath) . tail . lines $ contents)

-- read function with better error message
myRead :: (Read a) => String -> String -> a
myRead file s =  
  case [x | (x,t) <- reads s, ("","") <- lex t] of
    [x] -> x
    []  -> error ("Interface file: Cannot parse in .hx file " ++ file ++ " line " ++ s)
    y   -> error ("Interface file: Ambiguous parse of .hx file " ++ file)

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

readFirst :: String -> [FilePath] -> [FilePath] -> IO (FilePath,String)
readFirst modStr [] paths = do
  hPutStr stderr ("Fail: .hx file for module `" ++ modStr ++ "' not found in " ++ show paths ++ 
                  ". Give path with -I or -P.\n") 
  exitFailure
readFirst modStr (x:xs) paths =
  Control.Exception.catch (do finput <- readFile x
                              return (x,finput))
        (\ y -> (y :: Control.Exception.IOException) `seq` readFirst modStr xs paths)

-- `writeAuxFile' writes out the .hx file given this module's complete
-- parse tree.  The .hx file has an entry for every exported identifier, whether
-- defined in this module or reexported from imports.
-- Note that an entry may also contain names that are not exported, e.g. for a type its data constructors
-- or for a method its class.
writeAuxFile :: Flags -> FilePath -> Environment -> Module SrcSpanInfo -> IO ()
writeAuxFile flags filePath env mod = 
  writeFile filePath 
    ((showString "module " . (getId (getModuleNameFromModule mod) ++) . showChar '\n' .
     showLines (hxEnvironmentToList (exports (not (sDbgTrusted flags)) mod env))) "")
  where
  showLines :: Show a => [a] -> ShowS
  showLines = foldr (\x y-> shows x . showChar '\n' . y) id
