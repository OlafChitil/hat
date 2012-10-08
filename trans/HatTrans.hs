{- ---------------------------------------------------------------------------
This `main' function is basically just the front-end of the nhc98
compiler.  It parses the .hs source file, creates the .hx file, and
then stops immediately after writing a new transformed .hs file.
-} 
module Main where


import System.IO
import System.Environment(getArgs)
import System.Exit(exitWith,ExitCode(..))
import System.IO.Error(isAlreadyExistsError)
import Control.Monad(when)
import Data.List(isPrefixOf,intersperse,inits)
import System.Directory(doesDirectoryExist,createDirectory)

import Error
import Syntax
import Extra (Pos(..),mix,mixSpace,jRight,jLeft,noPos,strPos,showErr,mixLine
             ,pair,fst3,thd3,trace)
import AssocTree(listAT) -- for debugging output
import ParseCore (Parser(..),ParseBad(..),ParseError(..),ParseGood(..)
                 ,ParseResult(..),parseit)
import Flags (Flags(..),processArgs,pF,sUnderscore,sRealFile,sSourceFile,sUnlit
             ,sLex,sParse,sPrelude,sSrcDir,sIBound,sWrap
             ,sHatFileBase,sHatAuxFile,sTraceFns,sHatTransFile,sDbgTrusted)
import PrettyTraceId (prettyPrintTokenId,prettyPrintId,prettyPrintTraceId
                     ,ppModule,ppTopDecls,ppClassCodes)

import TokenId (TokenId(..),getUnqualified
               ,visible,t_Arrow,t_List,tPrelude,tminus,tnegate,tTrue)
import IdKind (IdKind(..))
import Id (Id)
import Lex (Lex,LexAnnot)  -- need show

import Unlit (unlit)
import Lexical (PosToken(..),PosTokenPre(..),LexState(..),lexical)
import Parse (parseProg)

import AuxFile (toAuxFile)
import AuxLabelAST (auxLabelSyntaxTree)
import TraceTrans (traceTrans,maybeStripOffQual)
import Wrapper (prepareWrapping)

import Paths_hat (getDataDir)


--import NonStdProfile
--beginPhase str = profile str (return ())
beginPhase str = return ()

-- some miscellaneous settings
primFlags = (False   -- bool is not the same as Word
	    ,False   -- && || not is not primitive
	    ,False   -- translate into prim only when strict
	    )

-- some nicer error handling
catchError :: Either b a -> String -> (b->String) -> IO a
catchError comp errmsg showErrors = do
    case comp of
        Left errs -> do pF True errmsg (showErrors errs)
                        exit
        Right a   -> return a

-- for Hugs, which cannot read commandline args using System.getArgs:
gmain cml = main' (words cml)

-- for all other compilers:
main = do
  args <- getArgs
  main' args

main' args = do
  let flagsArgs = processArgs args
  preludePath <- getDataDir
  let flags = flagsArgs{sPreludes = preludePath : (sPreludes flagsArgs)}
  let filename = sRealFile flags


  {- lex source code -}
  beginPhase "lex"
  mainChar	-- :: String
           <- catch (readFile filename) (can'tOpen filename) 
  lexdata	-- :: [PosToken]
           <- return (lexical (sUnderscore flags) (sSourceFile flags)
                              (if sUnlit flags 
                                then unlit (sSourceFile flags) mainChar 
                                else mainChar))
  pF (sLex flags) "Lexical" 
       (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata))


  {- parse source code -}
  beginPhase "parse"
  parsedPrg	-- :: Module TokenId
            <- catchError (parseit parseProg lexdata)
                          ("In file: "++sSourceFile flags) showErr
  pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedPrg) 

  {- Ensure we can write our output files. -}
  let hatDir = sSrcDir flags
  dir <- doesDirectoryExist hatDir
  when (not dir) (createDirectoriesRecursively hatDir)

  {- If wrapping rather than transforming, then prepare the parsed AST.
  -}
  parsedPrg <- if sWrap flags then return (prepareWrapping (sSourceFile flags) parsedPrg)
                              else return parsedPrg
  when (sWrap flags)
       (pF (sParse flags) "Wrap" (prettyPrintTokenId flags ppModule parsedPrg))

  {-
  -- Read and write auxiliary information files (for tracing).
  -- Then relabel the syntax tree with the auxiliary information.
  -- Then the tracing transformation itself is applied.
  -- The result is written to file (no redirection possible yet)
  -}
  let prg = implicitlyImportPrelude flags 
              (maybeStripOffQual "Prelude" parsedPrg)
  pF (sParse flags) "post-Prelude" (prettyPrintTokenId flags ppModule parsedPrg)
  -- hack to make full environment available when writing aux file
  -- (needed for handling partial expansion of type synonyms)
  (env,newprog) <- auxLabelSyntaxTree flags prg
  toAuxFile env flags (sHatAuxFile flags) prg -- waste space: hold on to prg

  when (sIBound flags)
    (print (listAT (fst env)))

  when (sTraceFns flags)
       (putStr (prettyPrintTraceId flags ppModule newprog)) -- debug
  let outputTree = (implicitlyImportPreludeBasic flags 
                     (maybeStripOffQual "Hat.Prelude"
                       (traceTrans (not (sDbgTrusted flags)) (sSourceFile flags) 
                         (sHatFileBase flags) newprog)))
  writeFile (sHatTransFile flags)
      (prettyPrintTokenId flags ppModule outputTree)
  putStrLn ("Wrote " ++ sHatTransFile flags)
  exitWith (ExitSuccess)


-- add implicit Prelude, if it is not imported explicitly and we 
-- are not currently transforming part of the Prelude.
implicitlyImportPrelude :: Flags -> Module TokenId -> Module TokenId
implicitlyImportPrelude flags
  (Module pos modId exports imports fixities decls) =
  Module pos modId exports imports' fixities decls
  where 
  imports' = 
    if (sPrelude flags && "Prelude" `isPrefixOf` getUnqualified modId) ||
       (any ((==) tPrelude . importedModule) imports)
      then imports 
      else Import (noPos,tPrelude) (Hiding []) : imports

-- import implementation of Prelude qualified
-- use this for transformation-introduced Prelude identifiers
-- even some internal identifiers that are not part of the Prelude
-- (e.g. for deriving of instances)
implicitlyImportPreludeBasic :: Flags -> Module TokenId -> Module TokenId
implicitlyImportPreludeBasic flags
  (Module pos modId exports imports fixities decls) =
  Module pos modId exports imports' fixities decls
  where 
  imports' = 
    if sPrelude flags && "Hat.Prelude" `isPrefixOf` getUnqualified modId
      then imports 
      else
        ImportQ (noPos,visible (reverse "Hat.PreludeBasic")) (Hiding []) 
        : imports


createDirectoriesRecursively path = do
    putStrLn ("Creating directories "++unwords paths)
    mapM_ safeCreate paths
  where paths = accum (wordsBy '/' path)
        accum xs = map (concat . intersperse "/") (tail (inits xs))
        wordsBy c s = let (w,s') = break (==c) s
                      in w: case s' of [] -> []; (_:s'') -> wordsBy c s''
        safeCreate path = catch (createDirectory path)
                                (\e-> if isAlreadyExistsError e then return ()
                                      else ioError e)

{- End Module Main ----------------------------------------------------------}
