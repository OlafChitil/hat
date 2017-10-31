module HatStack(hatStack) where -- HatStack main program

import LowLevel           (openHatFile,FileNode(..),nil,getParentNode
                          ,getErrorLoc,getErrorMessage
                          ,getSrcRef)
import SrcRef             (SrcRef(..),readSrcRef)
import SExp               (SExp(..),Label,fileNode2SExp,sExp2Doc,prune)
import System.Environment (getProgName)
import Foreign.C.String   (withCString)


-- first item is the expression, second is the position
type StackEntry = (SExp Label, Maybe (String, Int))

hatStack :: FilePath -> IO (Maybe (String, [StackEntry]))
hatStack hatfile = do
    prog <- getProgName
    withCString prog (\p-> withCString hatfile (openHatFile p))
    
    errloc <- getErrorLoc
    errmsg <- getErrorMessage
    if errloc == nil
        then return Nothing
        else return $ Just (errmsg, map toSExp $ takeWhile (/=nil) $ iterate getParentNode errloc)
    
    
toSExp :: FileNode -> (SExp Label, Maybe (String, Int))
toSExp node = let srcref = getSrcRef node
                  sr = readSrcRef srcref in
              ( fileNode2SExp 10 False True True ("l",node)
              , if (srcref==nil) then Nothing
                else Just (SrcRef.filename sr, SrcRef.line sr)
              )
