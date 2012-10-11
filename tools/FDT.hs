{-- | This module provides facilities for building Function Dependancy Trees, 
    | trusting of modules, functions and applications, displaying trees, and
    | finding parts of the tree.
    | --}
module FDT (buildFDT)
           where

import System.IO.Unsafe   (unsafePerformIO)
import Data.List          ((\\))
import Data.Set as Set    (Set(..),empty,insert,toList,unions)

import NodeExp  (NodeExp(..),flatEvalText,fullEvalText,findAppsMatching,getNode
                ,branches,result,removeResultCycles,removeNonResultCycles
                ,nodeExpForNode,isDataConstructor)
import Explore  (redexParent)
import LowLevel (FileNode(..))
import ADT      (ADT(..))
import Delta    (questionText)


type IndexEntry = (FileNode,ADT)

-- | Creates a function dependancy tree
buildFDT :: NodeExp -- ^ The trace to gather an FDT from -
                    -- ^ in the form of a NodeExp
         -> [ADT]
buildFDT = buildFDT' . replace

buildFDT' :: [IndexEntry] -> [ADT]
buildFDT' = zipWith tagTree (map (:[]) [1..]) . buildFromMap

buildFromMap xs =
  map (addChildren nonRoots) roots
  where
    (nonRoots,roots) = splitOn (parentIn $ map node xs) xs
    
    splitOn :: (a -> Bool) -> [a] -> ([a],[a])
    splitOn f [] = ([],[])
    splitOn f (x:xs)
      | f x       = let (ys,zs) = splitOn f xs in (x:ys,zs)
      | otherwise = let (ys,zs) = splitOn f xs in (ys,x:zs)
    
    parentIn ps (n,_) =
      (redexParent n) `elem` ps
    
    node :: IndexEntry -> FileNode
    node (_,Branch _ e _ _) = getNode e
    
    addChildren :: [IndexEntry] -> IndexEntry -> ADT
    addChildren ps n@(_,(Branch t exp f ocs)) = 
      Branch t exp f (ocs ++ cs)
      where
        cs = map (addChildren ncIEs) cIEs
        (cIEs,ncIEs) = splitOn (parentIn [node n]) ps
    

tagTree :: [Int] -> ADT -> ADT
tagTree t (Branch _ n f cs) = Branch t n f $ zipWith tagTree (map (:t) [1..]) cs
tagTree t (Cycle _ cs) = Cycle t $ zipWith tagTree (map (:t) [1..]) cs

concatZipWith f l1 l2= concat (zipWith f l1 l2)

fdtQuestion :: NodeExp -> Int -> String
fdtQuestion e w = questionText w (flatEvalText w) (fullEvalText w) e

replace :: NodeExp -> [(FileNode,ADT)]
replace n =
--  seq (unsafePerformIO $ putStrLn $ show nonRewritten) $
      map (applyMap nonRewritten) nonRewritten
  where
    makeMap :: [NodeExp] -> [IndexEntry]
    makeMap [] = []
    makeMap (x:xs) =
      (getNode $ getFunction x,Branch [] x (fdtQuestion x) []):makeMap xs
    
    nonRewritten = makeMap $ Set.toList $ gatherApps n
    
    gatherApps :: NodeExp -> Set NodeExp
    gatherApps a@(NExpApp n f _ r) =
      case r of
        NExpResultCycle n'
          -> if n == n' && isDataConstructor f
               then unions
               else added
        _ -> added
      where
        added = insert a unions
        unions = Set.unions $ map gatherApps $ branches a
    gatherApps a
      | null $ branches a = Set.empty
      | otherwise         = Set.unions $ map gatherApps $ branches a
    
    applyMap :: [IndexEntry] -> IndexEntry -> IndexEntry
    applyMap xs (n,Branch t exp _ cs) =
      (n,Branch t rExp (fdtQuestion rExp) cs)
      where
        rExp = rewrite xs exp
    
    rewrite :: [IndexEntry] -> NodeExp -> NodeExp
    rewrite xs (NExpApp n f as r) =
      NExpApp n (rewrite xs f) (map (findApps xs) as) (findApps xs r)
    rewrite xs (NExpConstUse n na r) = NExpConstUse n na (rewrite xs r)
    rewrite xs (NExpConstDef n na r) = NExpConstDef n na (rewrite xs r)
    rewrite xs (NExpCond n t c r) = NExpCond n t c (rewrite xs r)
    rewrite xs (NExpProjection n r) = NExpProjection n (rewrite xs r)
    rewrite xs (NExpForward n r) = NExpForward n (rewrite xs r)
    rewrite xs (NExpHidden n cs r) = NExpHidden n cs (rewrite xs r)
    rewrite xs y = y
    
    findApps :: [IndexEntry] -> NodeExp -> NodeExp
    findApps xs (NExpApp n f a r@(NExpResultCycle n'))
      | n == n'   = if isDataConstructor f
                      then NExpApp n (findApps xs f)
                                     (map (findApps xs) a)
                                     r
                      else let rs = map getNExp $ lookups n xs
                           in NExpExpanded n rs
      | otherwise = NExpApp n (findApps xs f)
                              (map (findApps xs) a)
                              r
    findApps xs (NExpApp n f a r) =
      NExpApp n (findApps xs f) (map (findApps xs) a) (findApps xs r)
    findApps xs (NExpConstUse n na r) = NExpConstUse n na (findApps xs r)
    findApps xs (NExpConstDef n na r) = NExpConstDef n na (findApps xs r)
    findApps xs (NExpCond n t c r) =
      NExpCond n t (findApps xs c) (findApps xs r)
    findApps xs (NExpFieldExp n exp ms) =
      NExpFieldExp n (findApps xs exp) (map doFAs ms)
      where
        doFAs (k,v) = (k,findApps xs v)
    findApps xs (NExpProjection n r) = NExpProjection n (findApps xs r)
    findApps xs (NExpForward n r) = NExpForward n (findApps xs r)
    findApps xs (NExpHidden n cs r) =
      NExpHidden n (map (findApps xs) cs) (findApps xs r)
    findApps xs (NExpLiteral n s) =
      if length rs == 0
        then NExpLiteral n s
        else NExpExpanded n rs
      where
        rs = map getNExp $ lookups n xs
    findApps xs (NExpIdentifier n na f) =
      if length rs == 0
        then NExpIdentifier n na f
        else NExpExpanded n rs
      where
        rs = map getNExp $ lookups n xs
    findApps _ x = x
    
    lookups :: Eq a => a -> [(a,b)] -> [b]
    lookups _ [] = []
    lookups x ((k,v):ys)
      | x == k    = v:lookups x ys
      | otherwise = lookups x ys
    
    getNExp :: ADT -> NodeExp
    getNExp (Branch _ exp _ _) = exp
    
dispApp :: NodeExp -> String
dispApp n = show n ++ " - " ++ (show $ redexParent $ getNode $ getFunction n) ++ "\n"

fdtChildren :: NodeExp -> [NodeExp]
fdtChildren exp@(NExpConstUse _ _ r) = fdtChildren r
fdtChildren exp@(NExpHidden _ _ _) =
  chldrn (getNode exp) (branches exp)
fdtChildren exp =
  (case result exp of
    NExpProjection n r -> chldrn (getNode pexp) (branches pexp)
                          where
                            pexp =   removeResultCycles
                                   $ removeNonResultCycles
                                   $ nodeExpForNode
                                   $ redexParent n
    _                  -> chldrn (getNode exp) (branches exp))

chldrn :: FileNode -> [NodeExp] -> [NodeExp]
chldrn n exps =
  (newApps ++ if null allApps
               then []
               else chldrn n (concatMap branches allApps))
  where
    newApps :: [NodeExp]
    newApps = concatMap (findAppsMatching (  (==n)
                                           . redexParent
                                           . getNode
                                           . getFunction))
                        exps
    allApps = concatMap (findAppsMatching (\_ -> True)) exps

getFunction :: NodeExp -> NodeExp
getFunction a@(NExpApp n f _ r) =
  case r of
    (NExpResultCycle n') ->
      if n == n'
        then a
        else getFunction f
    _                    ->
      getFunction f
getFunction a = a
