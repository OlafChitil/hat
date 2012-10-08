module ADT (ADT(..),displayTree,displayTrees,trustIO,trustConstant
           ,trustModule,trustApps,foldHiddens,leaves,subADTs,detectCycles
           ,trustUnevaluated,(/==),trustMatchingFunction) where

import NodeExp    (NodeExp(..),fullEval,flatEval,(===),finalResult,isomorphicIn)
import SExp       (QName(..))
import Data.Maybe (isJust,fromJust)

data ADT = Branch [Int] NodeExp (Int -> String) [ADT]
         | Cycle [Int] [ADT]

instance Show ADT where
  show = displayTree 80 (\_ s -> s)

tag (Branch t _ _ _) = t
tag (Cycle t _) = t

instance Eq ADT where
  (Branch _ n _ _) == (Branch _ n' _ _) = n == n'
  (Cycle _ xs) == (Cycle _ xs') = xs == xs'
  _ == _ = False
  
instance Ord ADT where
  compare (Branch _ n _ _) (Branch _ n' _ _) = compare n n'
  compare (Branch _ _ _ _) (Cycle _ _) = GT
  compare (Cycle _ _) (Branch _ _ _ _) = LT
  compare (Cycle _ _) (Cycle _ _) = EQ

(/==) :: ADT -> ADT -> Bool
(/==) e1 e2 = tag e1 /= tag e2

detectCycles :: [ADT] -> [ADT]
detectCycles =
  map $ detCycles []
  where
    detCycles :: [ADT] -> ADT -> ADT
    detCycles previous x@(Branch t exp q chldr) =
      let found = findExp (flatEval fullEval exp) previous
      in case found of
           [] -> Branch t exp q $ map (detCycles (x:previous)) chldr
           d  -> Cycle t d

    findExp :: NodeExp -> [ADT] -> [ADT]
    findExp x l =
      filter ((x ===) . (flatEval fullEval) . (\(Branch _ e _ _) -> e)) l

-- | Displays an EDT using a provided formatting.
displayTree :: Int -> (NodeExp -> String -> String) -> ADT -> String
displayTree w a (Branch _ exp q ts) =
  (a exp $ q w) ++ "\n" ++ (concat $ otherQuestions (w - 2) a ts)
displayTree w a (Cycle t cs) =
  if isJust q && isJust e
    then "CYCLE!\n" ++ (a (fromJust e) $ (fromJust q) (w - 2)) 
    else "CYCLE!\n"
  where
    (q,e) = findBranch cs
    findBranch [] = (Nothing,Nothing)
    findBranch (b@(Branch _ e n _):_) = (Just n,Just e)
    findBranch (_:xs) = findBranch xs

otherQuestions :: Int -> (NodeExp -> String -> String) -> [ADT] -> [String]
otherQuestions w a adts = 
     zipWith prependTree bTrees prepends
  ++ zipWith prependTree wTree wPrepends
  where
    prepends = " | " : prepends
    wPrepends = "   " : wPrepends
    trees = map (displayTree (w - 3) a) adts
    (bTrees,wTree) = splitAt (length trees - 1) trees

prependTree text prepend = 
  case theLines of
    []     -> "Error: An empty question was produced"
    (x:xs) -> unlines (  (" +-" ++ x) : (map (prepend ++) $ xs))
  where
    theLines = lines text

displayTrees :: Int -> (NodeExp -> String -> String) -> [ADT] -> String
displayTrees w a = concatMap (displayTree w a)

foldHiddens :: [ADT] -> [ADT]
foldHiddens = trustMatchingFunction False f
              where
                f (Branch _ e@(NExpHidden _ _ _) _ _) = True
                f _ = False

trustIO :: [ADT] -> [ADT]
trustIO =
  trustMatchingFunction False f
  where
    f (Branch _ e _ _) =
      (NExpIdentifier undefined
                      (Plain "{IO}")
                      undefined) `isomorphicIn` finalResult e

trustUnevaluated :: [ADT] -> [ADT]
trustUnevaluated = trustMatchingFunction False 
                                         (\(Branch _ e _ _)
                                            -> finalResult e === NExpUneval)

trustConstant :: QName -> [ADT] -> [ADT]
trustConstant name =
  trustMatchingFunction False f
  where
    f (Branch _ (NExpConstUse _ cName _) _ _) = name == cName
    f (Branch _ (NExpConstDef _ cName _) _ _) = name == cName
    f _ = False

trustModule :: String -> [ADT] -> [ADT]
trustModule mod =
  trustMatchingFunction False f
  where
    f (Branch _ (NExpApp _ (NExpIdentifier _ (Qualified mod' _) _) _ _) _ _)
      = mod == mod'
    f (Branch _ (NExpConstUse _ (Qualified mod' _) _) _ _)
      = mod == mod'
    f (Branch _ (NExpConstDef _ (Qualified mod' _) _) _ _)
      = mod == mod'
    f (Branch _ _ _ _) = False

trustApps :: [NodeExp] -> [ADT] -> [ADT]
trustApps apps trees =
  foldr trustApp trees apps
  where
    trustApp :: NodeExp -> [ADT] -> [ADT]
    trustApp exp = trustMatchingFunction False
                                         (  (compareApps exp)
                                          . (\(Branch _ e _ _) -> e))
    
    compareApps :: NodeExp -> NodeExp -> Bool
    compareApps (NExpApp _ f as _) (NExpApp _ f' as' _) =
         fullEval f === fullEval f'
      && all id (zipWith (===) (map fullEval as) (map fullEval as'))
    compareApps (NExpConstDef n _ _) (NExpConstDef n' _ _) = n == n'
    compareApps (NExpConstUse _ _ r) (NExpConstUse _ _ r') = compareApps r r'
    compareApps x y = False


trustMatchingFunction :: Bool -> (ADT -> Bool) -> [ADT] -> [ADT]
trustMatchingFunction _ _ [] = []
trustMatchingFunction tc f (b@(Branch t e q ch):os)
  | f b       = if tc 
                  then trustMatchingFunction tc f os
                  else    trustMatchingFunction tc f ch
                       ++ trustMatchingFunction tc f os
  | otherwise =   (Branch t e q (trustMatchingFunction tc f ch))
                : trustMatchingFunction tc f os 
trustMatchingFunction tc f (c@(Cycle t xs):os) =
  (Cycle t (trustMatchingFunction tc f xs)) : (trustMatchingFunction tc f os)

subADTs :: Maybe Int -> [ADT] -> [ADT]
subADTs _ []
  = []
subADTs (Just 0) _
  = []
subADTs (Just n) (e@(Branch _ _ _ []):others)
  = e:subADTs (Just n) others
subADTs (Just n) (e@(Branch _ _ _ chldrn):others)
  = e:(subADTs (Just (n-1)) chldrn ++ subADTs (Just n) others)
subADTs Nothing (e@(Branch _ _ _ []):others)
  = e:subADTs Nothing others
subADTs Nothing (e@(Branch _ _ _ chldrn):others)
  = e:(subADTs Nothing chldrn ++ subADTs Nothing others)
subADTs x ((Cycle _ e):others)
  = subADTs x others

leaves :: ADT -> [NodeExp]
leaves (Branch _ e _ []) = [e]
leaves (Branch _ e _ chldrn) = e:(concatMap leaves chldrn)
leaves (Cycle t e) = []
