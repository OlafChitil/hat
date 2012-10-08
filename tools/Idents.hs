{-# LANGUAGE EmptyDataDecls #-}

module Idents
  ( Ident(..)
  , collateIdents	-- :: IO ()
  , getAllIdents	-- :: IO ([Ident],[Ident],[Ident])
  , AllInfo		-- type AllInfo = [(String,[(String,Ident)])]
  , sortIdents		-- :: [Ident] -> AllInfo
  , showInfo		-- :: String -> AllInfo -> ShowS
  ) where

import LowLevel		(FileNode(..))
import Foreign.Ptr      (Ptr)
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc (free)
import Ident		(Ident(..),getIdentAt)
import Data.List
import HighlightStyle

---------------------------------------------------------------------
data Item

foreign import ccall "hat-names.h" getItemPtr  :: Ptr (Ptr Item) -> Int
							 -> IO (Ptr Item)
foreign import ccall "hat-names.h" itemIdent   :: Ptr Item -> IO FileNode
foreign import ccall "hat-names.h" itemArity   :: Ptr Item -> IO Int
foreign import ccall "hat-names.h" itemUses    :: Ptr Item -> IO Int
foreign import ccall "hat-names.h" itemPending :: Ptr Item -> IO Int
foreign import ccall "hat-names.h" itemThunks  :: Ptr Item -> IO Int

getOneIdent :: Ptr (Ptr Item) -> Int -> IO Ident
getOneIdent a n = do
  p <- getItemPtr a n
  atom  <- itemIdent p
  arity <- itemArity p
  uses  <- itemUses p
  pend  <- itemPending p
  th    <- itemThunks p
  ident <- getIdentAt atom
  free p
  return ident
	{ i_caf  = arity==0
	, i_uses = uses
	, i_pending = pend
	, i_thunks = th
	}

foreign import ccall "hat-names.h" collateIdents :: IO ()

foreign import ccall "hat-names.h" getGlobals  :: IO (Ptr (Ptr Item))
foreign import ccall "hat-names.h" getLocals   :: IO (Ptr (Ptr Item))
foreign import ccall "hat-names.h" getConstrs  :: IO (Ptr (Ptr Item))
foreign import ccall "hat-names.h" getGlobalsN :: IO Int
foreign import ccall "hat-names.h" getLocalsN  :: IO Int
foreign import ccall "hat-names.h" getConstrsN :: IO Int

getAllIdents :: IO ([Ident],[Ident],[Ident])
getAllIdents = do
  g  <- getGlobals
  gi <- getGlobalsN
  l  <- getLocals
  li <- getLocalsN
  c  <- getConstrs
  ci <- getConstrsN
  gs <- getAll g gi
  ls <- getAll l li
  cs <- getAll c ci
  return (gs, ls, cs)
  where
    getAll :: Ptr (Ptr Item) -> Int -> IO [Ident]
    getAll p i = mapM (getOneIdent p) [0..(i-1)]

---------------------------------------------------------------------
{-
-- InfoCmd describes how the data about identifiers should be presented.
-- For the moment, we just implement one style, AllByModule.
data InfoCmd
  = AllByModule | AllByAlpha | AllByFrequency
  | ModuleByAlpha String | ModuleByFrequency String
  | Individual (Maybe String) String
-}

--
--		[(modulename, [(idname,identifier info)])]
type AllInfo = [(String,[(String,Ident)])]

sortIdents :: [Ident] -> AllInfo
sortIdents is = foldr insMod [] is
  where
    insMod i []
	-- exclude identifiers with zero counts
        | (i_uses i == 0) && (i_pending i == 0) = []
        | otherwise = [(i_modname i, [(i_name i, i)])]
    insMod i xs@(o@(x,ys):rest)
        | (i_uses i == 0) && (i_pending i == 0)  = xs
        | i_isTraced i || not (i_caf i) =
          let m = i_modname i
              n = i_name i in
          case compare m x of
            LT -> (m,[(n,i)]): xs
         -- EQ -> (m, insertBy (\(n,i) (n',i')->compare n n') (n,i) ys): rest
            EQ -> (m, merge (n,i) ys): rest
            GT -> o : insMod i rest
        | otherwise = xs

merge (n,i) [] = [(n,i)]
merge (n,i) ((n',i'):nis) =
    case compare n n' of
      LT -> (n,i): (n',i'): nis
      EQ -> (n, combine i i'): nis
      GT -> (n',i'): merge (n,i) nis
  where
    combine i i' = i { i_uses    = i_uses i + i_uses i'
                     , i_pending = i_pending i + i_pending i'
                     , i_thunks  = i_thunks i + i_thunks i'
                     }

{-
showInfo :: AllInfo -> ShowS
showInfo xs = foldr (.) id (map showMod xs)
  where
    showMod (m,ns)  = showString (highlight [Foreground Blue] m) . nl .
                      foldr (.) id (map showIdent ns) . nl
    showIdent (n,i) = times 4 space
                      . (let pend = i_pending i in
                         if pend>0 then
                            showString (highlight [Foreground Red] (show pend))
                         else id)
                      . showString (highlight [Foreground Blue]
                                              (show (i_uses i)))
                      . space . showString n

    nl    = showChar '\n'
    space = showChar ' '
    times n x = foldr (.) id (replicate n x)
-}

showInfo :: String -> AllInfo -> ShowS
showInfo mod xs =
    case mod of
      "" -> foldr (.) id (map showMod xs)
      _  -> case lookup mod xs of
              Just v  -> showMod (mod,v)
              Nothing -> showString ("module "++mod++" not found.\n")
  where
    showMod (m,ns)  = showString m . trusted ns . nl
                      . columnate 80 (map showIdent ns) . nl
    showIdent (s,i) = (show (i_uses i), s)
    nl    = showChar '\n'
    space = showChar ' '
    times n x = foldr (.) id (replicate n x)
    trusted ((_,i):_) = if i_isTraced i then id else
                        showString (highlight [Foreground Blue] " (trusted)")


columnate :: Int -> [(String,String)] -> ShowS
columnate width ns =
  let largestN = maximum (map (length.fst) ns)
      largestS = maximum (map (length.snd) ns)
      numitems = length ns
      numcols  = width `div` (largestN+largestS+3)
      numlines = let x = numitems `mod` numcols
                     y = numitems `div` numcols in
                 if x==0 then y else y+1
      rjust n s = replicate (n-length s) ' ' ++ s
      ljust n s = s ++ replicate (n-length s) ' '
      shown (n,s) = highlight [Foreground Blue] (rjust largestN n)
                    ++" "++ ljust largestS s
      divided :: Int -> [a] -> [[a]]
      divided n [] = []
      divided n xs = let (a,b) = splitAt n xs in a: divided n b
      -- flipped :: [[a]] -> [[a]]
      flipped xss | all null xss = []
      flipped xss = map (safe head "") xss : flipped (map (safe tail []) xss)
      safe f e xs = if null xs then e else f xs
      oneline ns rest = foldr (.) id (map showString (intersperse "  " ns))
                        . showChar '\n' . rest
  in
  foldr oneline id (flipped (divided numlines (map shown ns)))


---------------------------------------------------------------------
