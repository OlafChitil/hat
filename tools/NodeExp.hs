-- Very Simple Expression (SExps, but even more simple!)
module NodeExp
  (NodeExp(..),(===),isIn,isomorphicIn,branches
  ,CondType(..)
  ,nodeExpForNode,result,finalResult,isDataConstructor
  ,compressClosures
  ,removeResultCycles,limitDepth
  ,fullEval,cutoffEval,condEval,flatEval,singleStepEval
  ,fullEvalText,flatEvalText
  ,children,findAppsMatching
  ,hideFunction
--  ,expandFunction
  ,nodeExp2SExp,removeNonResultCycles
  ,getNode,getFirstCaredNode
  ) where

import LowLevel hiding    ( nil )
import qualified LowLevel ( nil )
import Data.List          ( elemIndex , nub , null )
import SExp               ( SExp (..), SFixity(..), transFixity, QName(..)
                          , showQN, prettySExp)
import CommonUI           (Options(..),Keep(..))
import Explore            (redexParent)

import Data.List          ((\\))
import System.IO.Unsafe   (unsafePerformIO)

data CondType = IfCond | CaseCond | GuardCond deriving (Eq, Show)

-- Represents an ART file in it's raw form
data NodeExp
  = NExpApp         FileNode NodeExp [NodeExp] NodeExp
  | NExpConstUse    FileNode QName NodeExp
  | NExpConstDef    FileNode QName NodeExp
  | NExpIdentifier  FileNode QName SFixity
  | NExpConstructor FileNode QName SFixity
  | NExpLiteral     FileNode String
  | NExpCond        FileNode CondType NodeExp NodeExp
  | NExpFieldExp    FileNode NodeExp [(String, NodeExp)]
  | NExpProjection  FileNode NodeExp
  | NExpForward     FileNode NodeExp
  | NExpHidden      FileNode [NodeExp] NodeExp
  | NExpInterrupt
  | NExpUneval
  | NExpBottom
  | NExpNotShown
  | NExpResultCycle FileNode
  | NExpNonResultCycle FileNode
  -- An expanded function
  | NExpExpanded   FileNode [NodeExp]
  --deriving Show

data Infinite a = Inf
                | Num a deriving Eq

instance Ord a => Ord (Infinite a) where
  compare (Num x) (Num y) = compare x y
  compare (Num x) Inf = LT
  compare Inf (Num x) = GT
  compare Inf Inf = EQ

instance Show NodeExp where
  show (NExpApp n _ _ _)       = "App " ++ show n
  show (NExpConstUse n _ _)    = "ConstUse " ++ show n
  show (NExpConstDef n _ _)    = "ConstDef " ++ show n
  show (NExpIdentifier _ _ _)  = "ID"
  show (NExpConstructor _ _ _) = "Cons"
  show (NExpLiteral _ _)       = "Lit"
  show (NExpCond _ _ _ _)      = "Cond"
  show (NExpFieldExp _ _ _)    = "Field Exp"
  show (NExpProjection _ _)    = "Proj"
  show (NExpForward _ _)       = "Forw"
  show (NExpHidden n _ _)      = "Hid " ++ show n
  show NExpInterrupt           = "Int"
  show NExpUneval              = "Uneval"
  show NExpBottom              = "_|_"
  show NExpNotShown            = "NS"
  show (NExpResultCycle n)     = "RC" ++ show n
  show (NExpExpanded n _)      = "Expa" ++ show n

instance Eq NodeExp where
  (==) exp1 exp2
    = if isSimpleNodeType exp1 || isSimpleNodeType exp2
        then simpleComp exp1 exp2
        else (getNode exp1) == (getNode exp2)

instance Ord NodeExp where
  compare exp1 exp2
    = if isSimpleNodeType exp1
        then if isSimpleNodeType exp2
               then EQ
               else LT
        else if isSimpleNodeType exp2
               then GT
               else compare (getNode exp1) (getNode exp2)

branches :: NodeExp -> [NodeExp]
branches (NExpApp _ f as r) = f:r:as
branches (NExpConstUse _ _ r) = [r]
branches (NExpConstDef _ _ r) = [r]
branches (NExpCond _ _ c r) = [c,r]
branches (NExpFieldExp _ e ms) = e:(map snd ms)
branches (NExpProjection _ r) = [r]
branches (NExpForward _ r) = [r]
branches (NExpHidden _ c r) = r:c
branches _ = []

isDataConstructor :: NodeExp -> Bool
isDataConstructor (NExpIdentifier n na f) = False
isDataConstructor (NExpConstructor n na f) = True
isDataConstructor x
  | hasResult x = isDataConstructor (result x)
  | otherwise   = False

isIn :: NodeExp -> NodeExp -> Bool
isIn x y = gIn (==) x y

isomorphicIn :: NodeExp -> NodeExp -> Bool
isomorphicIn x y = gIn (===) x y

gIn :: (NodeExp -> NodeExp -> Bool) -> NodeExp -> NodeExp -> Bool
gIn f x y | x `f` y = True
gIn f x (NExpApp _ fun args res)
  =    gIn f x fun
    || any (gIn f x) args
    || gIn f x res
gIn f x (NExpConstUse _ _ res)
  = gIn f x res
gIn f x (NExpConstDef _ _ res)
  = gIn f x res
gIn f x (NExpCond _ _ cond res)
  = gIn f x cond || gIn f x res
gIn f x (NExpFieldExp _ _ mappings)
  = any (gIn f x . snd) mappings
gIn f x (NExpProjection _ res)
  = gIn f x res
gIn f x (NExpForward _ res)
  = gIn f x res
gIn f x (NExpHidden _ children res)
  =    (any (gIn f x) children)
    || gIn f x res
gIn _ _ _ = False

(===) :: NodeExp -> NodeExp -> Bool
(NExpApp _ fun args res) === (NExpApp _ fun2 args2 res2)
  = (fun === fun2) && (and $ zipWith (===) args args2) && (res === res2)
(NExpConstUse _ name res) === (NExpConstUse _ name2 res2)
  = (name == name2) && (res === res2)
(NExpConstDef _ name res) === (NExpConstDef _ name2 res2)
  = (name == name2) && (res === res2)
(NExpIdentifier _ name _) === (NExpIdentifier _ name2 _)
  = (name == name2)
(NExpConstructor _ name _) === (NExpConstructor _ name2 _)
  = (name == name2)
(NExpLiteral _ value) === (NExpLiteral _ value2)
  = value == value2
(NExpCond _ condType cond res) === (NExpCond _ condType2 cond2 res2)
  = (condType == condType2) && (cond === cond2) && (res === res2)
(NExpFieldExp _ name mappings) === (NExpFieldExp _ name2 mappings2)
  = (name == name2) && (and $ zipWith (\x y -> (fst x == fst y) && (snd x === snd y)) mappings mappings2)
(NExpProjection _ res) === (NExpProjection _ res2)
  = res === res2
(NExpHidden _ children res) === (NExpHidden _ children2 res2)
  = (and $ zipWith (===) children children2) && res === res2
(NExpForward _ res) === (NExpForward _ res2)
  = res === res2
(NExpExpanded _ apps) === (NExpExpanded _ apps2)
  = all id $ zipWith (===) apps apps2
NExpResultCycle _ === NExpResultCycle _ = True
NExpNonResultCycle _ === NExpNonResultCycle _ = True
NExpInterrupt === NExpInterrupt = True
NExpUneval === NExpUneval = True
NExpBottom === NExpBottom = True
NExpNotShown === NExpNotShown = True
_ === _ = False

getNode :: NodeExp -> FileNode
getNode (NExpApp node _ _ _) = node
getNode (NExpConstUse node _ _) = node
getNode (NExpConstDef node _ _) = node
getNode (NExpIdentifier node _ _) = node
getNode (NExpConstructor node _ _) = node
getNode (NExpCond node _ _ _) = node
getNode (NExpFieldExp node _ _) = node
getNode (NExpProjection node _) = node
getNode (NExpForward node _) = node
getNode (NExpHidden node _ _) = node
getNode (NExpResultCycle node) = node
getNode _ = LowLevel.nil

isSimpleNodeType :: NodeExp -> Bool
isSimpleNodeType NExpInterrupt = True
isSimpleNodeType NExpBottom = True
isSimpleNodeType NExpNotShown = True
isSimpleNodeType (NExpResultCycle _) = True
isSimpleNodeType _ = False

hasResult :: NodeExp -> Bool
hasResult NExpInterrupt = False
hasResult NExpUneval = False
hasResult NExpBottom = False
hasResult NExpNotShown = False
hasResult (NExpResultCycle _) = False
hasResult (NExpIdentifier _ _ _) = False
hasResult (NExpConstructor _ _ _) = False
hasResult (NExpLiteral _ _) = False
hasResult (NExpFieldExp _ _ _) = False
hasResult _ = True

result :: NodeExp -> NodeExp
result (NExpApp _ _ _ res) = res
result (NExpConstUse _ _ res) = res
result (NExpConstDef _ _ res) = res
result (NExpCond _ _ _ res) = res
result (NExpProjection _ res) = res
result (NExpForward _ res) = res
result (NExpHidden _ _ res) = res
result x = x

finalResult :: NodeExp -> NodeExp
finalResult (NExpApp _ _ _ res) = finalResult res
finalResult (NExpConstUse _ _ res) = finalResult res
finalResult (NExpConstDef _ _ res) = finalResult res
finalResult (NExpCond _ _ _ res) = finalResult res
finalResult (NExpProjection _ res) = finalResult res
finalResult (NExpForward _ res) = finalResult res
finalResult (NExpHidden _ _ res) = finalResult res
finalResult x = x

simpleComp :: NodeExp -> NodeExp -> Bool
simpleComp NExpInterrupt NExpInterrupt = True
simpleComp NExpBottom NExpBottom = True
simpleComp NExpNotShown NExpNotShown = True
simpleComp (NExpResultCycle x) (NExpResultCycle y) = x == y
simpleComp _ _ = False

nodeExpForNode :: FileNode -> NodeExp
nodeExpForNode node =
--  seq (unsafePerformIO $ putStrLn $ ("NodExp: " ++ show node)) $ 
  if node == LowLevel.unevaluated then NExpUneval
  else if node == LowLevel.entered then NExpBottom
  else if node == LowLevel.interrupted then NExpInterrupt
  else if node == LowLevel.nil then NExpNotShown
  else
    case simpleNodeType node of
      NodeAtom ->
        case nodeType node of
          AtomConstructor ->
            NExpConstructor node
                            (Qualified (getAtomMod node) (getAtom node))
                            (transFixity (getAtomFixity node))
          _ ->
            NExpIdentifier node
                           (Qualified (getAtomMod node) (getAtom node))
                           (transFixity (getAtomFixity node))
      NodeApplication ->
        NExpApp node funExp argExps resExp
        where
          funExp = nodeExpForNode function
          argExps = map nodeExpForNode args
          resExp = nodeExpForNode (peekResult node)
          (function:args) = peekSubExprs node
      NodeBasicValue ->
        NExpLiteral node (getValue node)
      NodeCAF ->
        case nodeType node of
          ExpConstUse ->
            NExpConstUse node
                         (Qualified (getValueMod node) (getValue node))
                         (nodeExpForNode $ peekResult node)
          ExpConstDef ->
            NExpConstDef node
                         (Qualified (getValueMod node)
                                    (getValue node))
                         (nodeExpForNode $ peekResult node)
      NodeConditional ->
        let
          condType = case nodeType node of
            ExpIf -> IfCond
            ExpCase -> CaseCond
            ExpGuard -> GuardCond
        in
          NExpCond node
                   condType
                   (nodeExpForNode (mHead "NodeConditional" (peekSubExprs node)))
                   (nodeExpForNode (peekResult node))
      NodeIdentifier->
        NExpIdentifier node
                       (Qualified mod id)
                       (grabFixity mod id)
        where
          mod = getValueMod node
          id = getValue node
          grabFixity m i =
            case i of
              "."   | m == "Prelude" -> SAssoc 9 i
              "++"  | m == "Prelude" -> SAssoc 5 i
              "&&"  | m == "Prelude" -> SAssoc 3 i
              "||"  | m == "Prelude" -> SAssoc 2 i
              "*"   | m == "Prelude" -> SAssoc 7 i
              "+"   | m == "Prelude" -> SAssoc 6 i
              ">>"  | m == "Prelude" -> SAssoc 1 i
              ">>=" | m == "Prelude" -> SAssoc 1 i
              _                      -> transFixity (getFixity node)
      NodeSpecial ->
        case nodeType node of
          ExpProjection ->
            NExpProjection node
                           (nodeExpForNode (peekResult node))
          ExpHidden ->
            NExpHidden node
                       (map nodeExpForNode $ hiddenChildren node)
                       (nodeExpForNode (peekResult node))
          ExpForward ->
            NExpForward node (nodeExpForNode (peekResult node))
      NodeSugar ->
        case nodeType node of
          ExpDoStmt ->
            NExpLiteral node "{do stmt}"
          ExpFieldUpdate ->
            NExpFieldExp node exp (zip (getFieldLabels node) range)
            where
              (exp:range) = map nodeExpForNode (peekSubExprs node)

removeResultCycles :: NodeExp -> NodeExp
removeResultCycles = remCycles []
  where
    remCycles :: [FileNode] -> NodeExp -> NodeExp
    remCycles xs e@(NExpApp node fun args res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpApp node
                       (remCycles (node:xs) fun)
                       (map (remCycles (node:xs)) args)
                       (remCycles (node:xs) res)
    remCycles xs e@(NExpConstUse node def res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if  node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpConstUse node def (remCycles (node:xs) res)
    remCycles xs e@(NExpConstDef node def res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if  node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpConstDef node def (remCycles (node:xs) res)
    remCycles xs e@(NExpCond node condType cond res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpCond node
                        condType
                        (remCycles (node:xs) cond)
                        (remCycles (node:xs) res)
    remCycles xs e@(NExpProjection node res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpProjection node
                              (remCycles (node:xs) res)
    remCycles xs e@(NExpForward node res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpForward node (remCycles (node:xs) res)
    remCycles xs e@(NExpHidden node children res)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        if node `elem` xs
          then (NExpResultCycle (getNode res))
          else NExpHidden node
                          (map (remCycles (node:xs)) children)
                          (remCycles (node:xs) res)
    remCycles xs e@(NExpFieldExp node exp mappings)
      = -- seq (unsafePerformIO $ putStrLn ("remCycles: " ++ show e)) $
        NExpFieldExp node (remCycles (node:xs) exp) (map remCycs mappings)
        where
          remCycs :: (String, NodeExp) -> (String, NodeExp)
          remCycs (x,y) = (x, remCycles (node:xs) y)
    remCycles xs y = y

limitDepth :: Int -> NodeExp -> NodeExp
limitDepth 0 _ = NExpNotShown
limitDepth n (NExpApp node fun args res)
  = NExpApp node
            (limitDepth (n-1) fun)
            (map (limitDepth (n-1)) args)
            (limitDepth (n-1) res)
limitDepth n (NExpConstUse node def res)
  = NExpConstUse node def (limitDepth (n-1) res)
limitDepth n (NExpConstDef node def res)
  = NExpConstDef node def (limitDepth (n-1) res)
limitDepth n (NExpCond node condType cond res)
  = NExpCond node
             condType
             (limitDepth (n-1) cond)
             (limitDepth (n-1) res)
limitDepth n (NExpFieldExp node exp mappings)
  = NExpFieldExp node (limitDepth (n-1) exp) (map limitMapDepth mappings)
    where
      limitMapDepth :: (String, NodeExp) -> (String, NodeExp)
      limitMapDepth (x, y) = (x, limitDepth (n-1) y)
limitDepth n (NExpProjection node res)
  = NExpProjection node (limitDepth (n-1) res)
limitDepth n (NExpForward node res)
  = NExpForward node (limitDepth (n-1) res)
limitDepth n (NExpHidden node children res)
  = NExpHidden node
               (map (limitDepth (n-1)) children)
               (limitDepth (n-1) res)
limitDepth n x = x

compressClosures :: NodeExp -> NodeExp
compressClosures (NExpApp node
                          (NExpApp innerNode innerFun innerArgs innerRes)
                          args
                          res) =
  NExpApp node
          innerFun
          (map compressClosures (innerArgs ++ args))
          (compressClosures res)
compressClosures (NExpApp node fun args res) =
  NExpApp node
          (compressClosures fun)
          (map compressClosures args)
          (compressClosures res)
compressClosures (NExpConstUse node def res) =
  NExpConstUse node def (compressClosures res)
compressClosures (NExpConstDef node def res) =
  NExpConstDef node def (compressClosures res)
compressClosures (NExpCond node condType cond res) =
  NExpCond node
           condType
           (compressClosures cond)
           (compressClosures res)
compressClosures (NExpFieldExp node exp mappings) =
  NExpFieldExp node
               (compressClosures exp)
               (map compressMapping mappings)
  where
    compressMapping :: (String, NodeExp) -> (String, NodeExp)
    compressMapping (x, y) = (x, compressClosures y)
compressClosures (NExpProjection node res) =
  NExpProjection node (compressClosures res)
compressClosures (NExpForward node res) =
  NExpForward node (compressClosures res)
compressClosures (NExpHidden node children res) =
  NExpHidden node
             (map compressClosures children)
             (compressClosures res)
compressClosures x = x

expText :: Int -> NodeExp -> String
expText w = (prettySExp "" w (Options {cutoffDepth=30
                                      ,unevalMode=False
                                      ,stringSugar=True
                                      ,listSugar=True
                                      ,recursiveMode=True
                                      ,colourBracks=False
                                      ,equations=True
                                      ,showQual=False
                                      ,filterMode=Unique})) .
            nodeExp2SExp . removeNonResultCycles

flatEvalText :: Int -> NodeExp -> String
flatEvalText w = (expText w) . (flatEval fullEval)

fullEvalText :: Int -> NodeExp -> String
fullEvalText w = (expText w) . fullEval

fullEval :: NodeExp -> NodeExp
fullEval = condEval (\x -> (not $ isCycle x) && (not $ isTotallyHidden x))

isTotallyHidden :: NodeExp -> Bool
isTotallyHidden exp =
  case res of
    NExpResultCycle n    -> let nType = nodeType n
                            in nType /= ExpValueApp && nType /= ExpApp
    NExpNonResultCycle n -> let nType = nodeType n
                            in nType /= ExpValueApp && nType /= ExpApp
    _                    -> False
  where
    res = finalResult exp

isCycle :: NodeExp -> Bool
isCycle (NExpResultCycle _) = True
isCycle (NExpNonResultCycle _) = True
isCycle _ = False
{-  | not (hasResult exp) = True
  | otherwise
    = case result exp of
        NExpResultCycle _ -> False
        _                 -> True -}

flatEval :: (NodeExp -> NodeExp) -> NodeExp -> NodeExp
flatEval buildSubs (NExpApp node fun args res)
  = NExpApp node funExp argExps resExp
      where
        funExp = case fun of
                   (NExpApp _ _ _ _) -> flatEval buildSubs fun
                   _                 -> buildSubs fun
        argExps = map buildSubs args
        resExp = buildSubs res
flatEval buildSubs (NExpConstUse node def res)
  = NExpConstUse node def (buildSubs res)
flatEval buildSubs (NExpConstDef node def res)
  = NExpConstDef node def (buildSubs res)
flatEval buildSubs (NExpCond node condType cond res)
  = NExpCond node condType (buildSubs cond) (buildSubs res)
flatEval buildSubs (NExpFieldExp node exp mappings)
  = NExpFieldExp node procExp procMappings
      where
        procExp = buildSubs exp
        procMappings = map buildSub mappings
        buildSub :: (String, NodeExp) -> (String, NodeExp)
        buildSub (x, y) = (x, buildSubs y)
flatEval buildSubs (NExpProjection node res)
  = buildSubs res
flatEval buildSubs (NExpForward node res)
  = buildSubs res
flatEval buildSubs (NExpHidden node children res)
  = NExpLiteral node "{?}"
flatEval _ x = x

-- Will chase result pointers until it hits the specified offest.
cutoffEval :: FileNode -> NodeExp -> NodeExp
cutoffEval cutoff node = condEval (lessThanEqNode cutoff) node

singleStepEval :: NodeExp -> NodeExp
singleStepEval node =
  n
  where
    (n,t) = singleStepEval' t node
    
    singleStepEval' :: Infinite FileNode
                    -> NodeExp
                    -> (NodeExp, Infinite FileNode)
    singleStepEval' c (NExpApp n f as r) =
      case r of
        NExpResultCycle _ ->
          (NExpApp n fe ase re, minimum (fm:rm:asm))
        _                 ->
          (if (Num n) <= c then re else NExpApp n fe ase re
          ,minimum ((Num n):fm:rm:asm))
      where
        (fe,fm) = singleStepEval' c f
        (ase,asm) = unzip . map (singleStepEval' c) $ as
        (re,rm) = singleStepEval' c r
    singleStepEval' c (NExpConstUse n nm r) = singleStepEval' c r
    singleStepEval' c (NExpConstDef n nm r) = singleStepEval' c r
    singleStepEval' c (NExpCond n t cn r) =
      case r of
        NExpResultCycle _ ->
          (NExpCond n t cne re, min cnm rm)
        _                 ->
          (if (Num n) <= c then re else NExpCond n t cne re
          ,min (Num n) (min cnm rm))
      where
        (cne,cnm) = singleStepEval' c cn
        (re,rm) = singleStepEval' c r
    singleStepEval' c (NExpFieldExp n e m) =
      (NExpFieldExp n ee (zip (map fst m) me), minimum (em:mm))
      where
        (ee,em) = singleStepEval' c e
        (me,mm) = unzip . map (singleStepEval' c . snd) $ m
    singleStepEval' c (NExpProjection n r) = singleStepEval' c r
    singleStepEval' c (NExpForward n r)    = singleStepEval' c r
    singleStepEval' c (NExpHidden _ _ r)   = singleStepEval' c r
    singleStepEval' c exp = (exp, Inf)

lessThanEqNode :: FileNode -> NodeExp -> Bool
lessThanEqNode node (NExpResultCycle _) = False
lessThanEqNode node exp
  = if caredNode == Nothing then True
    else (unMaybe caredNode) <= node
    where
      caredNode = (getFirstCaredNode exp)

unMaybe :: Maybe a -> a
unMaybe (Just x) = x

fetchOffset :: NodeExp -> FileNode
fetchOffset (NExpApp node _ _ _) = node
fetchOffset (NExpConstUse node _ _) = node
fetchOffset (NExpConstDef node _ _) = node
fetchOffset (NExpIdentifier node _ _) = node
fetchOffset (NExpConstructor node _ _) = node
fetchOffset (NExpLiteral node _) = node
fetchOffset (NExpCond node _ _ _) = node
fetchOffset (NExpFieldExp node _ _) = node
fetchOffset (NExpProjection node _) = node
fetchOffset (NExpForward _ res) = fetchOffset res
fetchOffset (NExpHidden node _ _) = node

getFirstCaredNode :: NodeExp -> Maybe FileNode
getFirstCaredNode (NExpApp node _ _ res)
  = case res of
      (NExpResultCycle _)   -> Nothing
      NExpUneval            -> Nothing
      otherwise             -> Just node
getFirstCaredNode (NExpConstUse node _ res)
  = case res of
      (NExpResultCycle _)   -> Nothing
      NExpUneval            -> Nothing
      otherwise             -> Just node
getFirstCaredNode (NExpConstDef node _ res)
  = case res of
      (NExpResultCycle _)   -> Nothing
      NExpUneval            -> Nothing
      otherwise             -> Just node
getFirstCaredNode (NExpIdentifier node _ _) = Just node
getFirstCaredNode (NExpConstructor node _ _) = Just node
getFirstCaredNode (NExpLiteral node _) = Just node
getFirstCaredNode (NExpCond node _ _ _) = Just node
getFirstCaredNode (NExpFieldExp node _ _) = Just node
getFirstCaredNode (NExpProjection node _) = Just node
getFirstCaredNode (NExpForward _ res) = getFirstCaredNode res
getFirstCaredNode (NExpHidden _ _ res) = getFirstCaredNode res
getFirstCaredNode _ = Nothing

-- Will chase result pointers until the predicate passed returns False.
condEval :: (NodeExp -> Bool) -> NodeExp -> NodeExp
condEval pred nExp@(NExpApp node fun args res)
  = if pred res
      then condEval pred res
      else NExpApp node
                   (condEval pred fun)
                   (map (condEval pred) args)
                   (condEval pred res)
condEval pred nExp@(NExpConstUse node def res)
  = if pred res
      then condEval pred res
      else NExpConstUse node def (condEval pred res)
condEval pred nExp@(NExpConstDef node def res)
  = if pred res
      then condEval pred res
      else NExpConstDef node def (condEval pred res)
condEval pred nExp@(NExpCond node condType cond res)
  = if pred res
      then condEval pred res
      else NExpCond node
                    condType
                    (condEval pred cond)
                    (condEval pred res)
condEval pred nExp@(NExpFieldExp node exp mappings)
  = NExpFieldExp node evalExp evalMappings
    where
      evalExp = condEval pred exp
      evalMappings = map evalMapping mappings
      evalMapping :: (String, NodeExp) -> (String, NodeExp)
      evalMapping (x,y) = (x, condEval pred y)
condEval pred nExp@(NExpProjection node res)
  = condEval pred res
condEval pred nExp@(NExpForward _ res)
  = condEval pred res
condEval pred nExp@(NExpHidden node children res)
  = if pred res
      then condEval pred res
      else NExpHidden node
                      (map (condEval pred) children)
                      (condEval pred res)
condEval _ x = x

{-cutoffFindMinOffset :: FileNode -> NodeExp -> Maybe FileNode
cutoffFindMinOffset cutoff node
  = if offsets == [] then Nothing
    else Just (minimum offsets)
    where offsets = (map fetchOffset
                         (filter (not . isSimpleNodeType)
                                 (cutoffFindNodes cutoff node)))

cutoffFindNodes :: FileNode -> NodeExp -> [NodeExp]
cutoffFindNodes cutoff = condFindNodes (lessThanEqNode cutoff)

condFindNodes :: (NodeExp -> Bool) -> NodeExp -> [NodeExp]
condFindNodes pred nExp@(NExpApp _ fun args res)
  = if pred nExp
      then ((condFindNodes pred res) ++ (condFindNodes pred fun)
            ++ (foldr ((++) . (condFindNodes pred)) [] args))
      else [nExp]
condFindNodes pred nExp@(NExpConstUse _ def res)
  = if pred nExp
      then condFindNodes pred res
      else [nExp]
condFindNodes pred nExp@(NExpConstDef _ def res)
  = if pred nExp
      then condFindNodes pred res
      else [nExp]
condFindNodes pred nExp@(NExpCond _ condType cond res)
  = if pred nExp
      then (condFindNodes pred res) ++ (condFindNodes pred cond)
      else [nExp]
condFindNodes pred nExp@(NExpFieldExp _ exp mappings)
  = if pred nExp
      then (condFindNodes pred exp)
            ++ (foldr ((++) . (condFindNodes pred) . snd) [] mappings)
      else [nExp]
condFindNodes pred nExp@(NExpProjection _ res)
  = (condFindNodes pred res)
condFindNodes pred nExp@(NExpForward _ res)
  = (condFindNodes pred res)
condFindNodes pred nExp@(NExpHidden _ children res)
  = if pred nExp
      then (   (condFindNodes pred res)
            ++ (foldr ((++) . (condFindNodes pred)) [] children))
      else [nExp]
condFindNodes pred x = if pred x then [] else [x]-}

-- Trusts a function i.e. follows the result pointer for each application
-- of the function and replaces the node.  The result VSExp is created using
-- the import function.
hideFunction :: String -> NodeExp -> NodeExp
-- Hiding requires three conditions:
--  1. The function part of an application is fully evaluated
--  2. The names of the function is that being hidden
--  3. The node is not a value application
-- If this is met, we remove the app and replace with it's result, otherwise
-- we leave the expression as is and recurse.
hideFunction name app@(NExpApp node fun@(NExpIdentifier _ funName fix) args res)
  = if ((showQN True funName) == name || (showQN False funName) == name)
       && nodeType node /= ExpValueApp
      then hideFunction name res
      else NExpApp node
                   fun
                   (map (hideFunction name) args)
                   (hideFunction name res)
hideFunction name app@(NExpApp node fun args res)
  = NExpApp node
            (hideFunction name fun)
            (map (hideFunction name) args)
            (hideFunction name res)
hideFunction "if" ifCon@(NExpCond node IfCond _ res)
  = hideFunction "if" res
hideFunction "case" ifCon@(NExpCond node CaseCond _ res)
  = hideFunction "case" res
hideFunction "|" ifCon@(NExpCond node GuardCond cond res)
  = hideFunction "|" res
hideFunction name ifCon@(NExpCond node condType cond res)
  = NExpCond node condType (hideFunction name cond) (hideFunction name res)
hideFunction name fieldExp@(NExpFieldExp node exp mappings)
  = NExpFieldExp node (hideFunction name exp) (map hideMapping mappings)
    where
      hideMapping :: (String, NodeExp) -> (String, NodeExp)
      hideMapping (x, y) = (x, hideFunction name y)
hideFunction _ x = x

{-expandFunction :: NodeExp -> NodeExp -> NodeExp
expandFunction f a@(NExpApp off g as r) =
  NExpApp off (replaceIDs f maps g) (map (replaceIDs f maps) as) r
  where
    maps = applications f r
    replaceIDs f maps id@(NExpIdentifier off na fix)
      | id == f = NExpExpanded (getNode g) maps
      | otherwise
        = NExpIdentifier off na fix
    replaceIDs f maps (NExpApp off g as r) =
      NExpApp off (replaceIDs f maps g) (map (replaceIDs f maps) as) r
    replaceIDs f maps (NExpConstUse off n r) =
      NExpConstUse off n (replaceIDs f maps r)
    replaceIDs f maps (NExpConstDef off n r) =
      NExpConstDef off n (replaceIDs f maps r)
    replaceIDs f maps (NExpCond off ct c r) =
      NExpCond off ct (replaceIDs f maps c) (replaceIDs f maps r)
    replaceIDs f maps (NExpFieldExp off n ps) =
      NExpFieldExp off
                   (replaceIDs f maps n)
                   (map (\(x,y) -> (x, replaceIDs f maps y)) ps)
    replaceIDs f maps (NExpProjection off r) =
      NExpProjection off (replaceIDs f maps r)
    replaceIDs f maps (NExpForward off r) =
      NExpForward off (replaceIDs f maps r)
    replaceIDs f maps (NExpHidden off cs r) =
      NExpHidden off (map (replaceIDs f maps) cs) (replaceIDs f maps r)
    replaceIDs f maps (NExpExpanded off ms) =
      NExpExpanded off (map (replaceIDs f maps) ms)
    replaceIDs _ _ x = x
expandFunction _ x = x-}

applications :: NodeExp -> NodeExp -> [NodeExp]
applications x y =
  if isSimpleNodeType x
    then []
    else apps (getNode x) y
  where
    apps :: FileNode -> NodeExp -> [NodeExp]
    apps f y =
      if isSimpleNodeType y
        then []
        else
          case y of
            NExpApp n fun _ _ -> if f == getNode fun
                                   then y:(concatMap (apps f) (branches y))
                                   else concatMap (apps f) (branches y)
            _                 -> concatMap (apps f) (branches y)

-- removes cycles in the function and argument pointers
removeNonResultCycles :: NodeExp -> NodeExp
removeNonResultCycles = remCycles []
  where
    remCycles :: [FileNode] -> NodeExp -> NodeExp
    remCycles visited (NExpApp node fun args res)
      = if node `elem` visited
          then NExpNonResultCycle node
          else NExpApp node
                       (remCycles (node:visited) fun)
                       (map (remCycles (node:visited)) args)
                       res
    remCycles visited (NExpCond node condType cond res)
      = if node `elem` visited
          then NExpNonResultCycle node
          else NExpCond node
                        condType
                        (remCycles (node:visited) cond)
                        (remCycles (node:visited) res)
    remCycles visited (NExpFieldExp node exp mappings)
      = if node `elem` visited
          then NExpNonResultCycle node
          else NExpFieldExp node
                            (remCycles (node:visited) exp)
                            (map remCycs mappings)
        where
          remCycs :: (String, NodeExp) -> (String, NodeExp)
          remCycs (x,y) = (x, remCycles (node:visited) y)
    remCycles _ y = y

children :: NodeExp -> [NodeExp]
children exp@(NExpConstUse _ _ r) = children r
children exp@(NExpHidden _ _ _) =
  chldrn (getNode exp) (branches exp)
children exp =
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
  (newApps ++ if null newApps
               then []
               else chldrn n (concatMap branches newApps))
  where
    newApps :: [NodeExp]
    newApps = concatMap (findAppsMatching ((==n) . redexParent . getNode))
                        exps

findAppsMatching :: (NodeExp -> Bool) -> NodeExp -> [NodeExp]
findAppsMatching f exp@(NExpApp _ fu as r) =
  if f exp then case r of
                  NExpResultCycle _
                    -> concatMap (findAppsMatching f) $ (fu:as)
                  NExpNonResultCycle _
                    -> concatMap (findAppsMatching f) $ (fu:as)
                  _ -> [exp]
           else []
findAppsMatching f exp@(NExpConstUse _ _ r) =
  if f exp then
    if isTotallyHidden r
      then []
      else case r of
             NExpResultCycle _    -> []
             NExpNonResultCycle _ -> []
             _                    -> [exp]
  else []
findAppsMatching f exp@(NExpConstDef _ _ _) =
  if f exp then concatMap (findAppsMatching f) (branches exp)
           else []
findAppsMatching f exp@(NExpCond _ _ _ _) =
  if f exp then concatMap (findAppsMatching f) (branches exp)
           else []
findAppsMatching f exp@(NExpProjection _ _) =
  if f exp then concatMap (findAppsMatching f) (branches exp)
           else []
findAppsMatching f exp@(NExpIdentifier _ _ _) =
  if f exp then [exp]
           else []
findAppsMatching f exp@(NExpConstructor _ _ _) =
  if f exp then [exp]
           else []
findAppsMatching f exp@(NExpHidden n c r) =
  [exp]
findAppsMatching f exp@(NExpResultCycle _) = []
findAppsMatching f exp@(NExpNonResultCycle _) = []
findAppsMatching f exp =
  concatMap (findAppsMatching f) $ branches exp

nodeExp2SExp :: NodeExp -> SExp String
nodeExp2SExp app@(NExpApp _ fun args _)
  = if isAppString app
      then SString "" (buildString app) False
      else SApp "" ((nodeExp2SExp fun):(map nodeExp2SExp args))
nodeExp2SExp (NExpConstUse _ id _)
  = SId "" id SInfixDefault
nodeExp2SExp (NExpConstDef _ id _)
  = SId "" id SInfixDefault
nodeExp2SExp (NExpIdentifier _ id fix)
  = SId "" id fix
nodeExp2SExp (NExpConstructor _ id fix)
  = SId "" id fix
nodeExp2SExp (NExpLiteral _ lit)
  = SLiteral "" lit
nodeExp2SExp (NExpCond _ IfCond cond res)
  = SIf "" (nodeExp2SExp cond) (Just (nodeExp2SExp res))
nodeExp2SExp (NExpCond _ CaseCond cond res)
  = SCase "" (nodeExp2SExp cond) (Just (nodeExp2SExp res))
nodeExp2SExp (NExpCond _ GuardCond cond res)
  = SGuard "" (nodeExp2SExp cond) (Just (nodeExp2SExp res))
nodeExp2SExp (NExpFieldExp _ exp mappings)
  = SFieldExpr ""
               (nodeExp2SExp exp)
               (fst namesAndResults)
               (map nodeExp2SExp (snd namesAndResults))
    where namesAndResults = unzip mappings
nodeExp2SExp (NExpProjection _ res)
  = nodeExp2SExp res
nodeExp2SExp (NExpForward _ res)
  = nodeExp2SExp res
nodeExp2SExp (NExpHidden _ _ res)
  = nodeExp2SExp res
nodeExp2SExp NExpInterrupt = SInterrupted ""
nodeExp2SExp NExpBottom = SBottom ""
nodeExp2SExp NExpUneval = SUnevaluated ""
nodeExp2SExp NExpNotShown = SCut ""
nodeExp2SExp (NExpResultCycle _) = SInfinite ""
nodeExp2SExp (NExpExpanded n apps) = --SString "" (show n) False --
  SFiniteMap "" (makeMaps apps)

makeMaps :: [NodeExp] -> [([SExp String],SExp String)]
makeMaps =
  map makeMap
  where
    makeMap :: NodeExp -> ([SExp String],SExp String)
    makeMap (NExpApp _ _ as r) =
      (map (nodeExp2SExp . fullEval) as, nodeExp2SExp $ fullEval r)

isAppString :: NodeExp -> Bool
isAppString (NExpApp _ (NExpApp _ fun@(NExpConstructor _ name _) args1 _) args2 _)
  = if name == (Qualified "Prelude" ":") then
      if argNode /= LowLevel.nil &&
         argNode /= LowLevel.unevaluated &&
         argNode /= LowLevel.entered &&
         argNode /= LowLevel.interrupted then
        ((nodeType argNode) == ExpChar)
         && (not $ null $ tail args)
         && ((isAppString (mHead "isAppString 1.2" (tail args)))
             || isEmptyList (mHead "isAppString 1.3" (tail args)))
      else False
    else False
    where
      args = args1 ++ args2
      argNode = (getNode (mHead "isAppString 1.1" args))
isAppString (NExpApp _ fun@(NExpConstructor _ name _) args _)
  = if name == (Qualified "Prelude" ":") then
      if argNode /= LowLevel.nil &&
         argNode /= LowLevel.unevaluated &&
         argNode /= LowLevel.entered &&
         argNode /= LowLevel.interrupted then
        ((nodeType argNode) == ExpChar)
         && (not $ null $ tail args)
         && ((isAppString (mHead "isAppString 2.2" (tail args)))
             || isEmptyList (mHead "isAppString 2.3" (tail args)))
      else False
    else False
    where
      argNode = (getNode (mHead "isAppString 2.1" args))
isAppString exp = False

buildString :: NodeExp -> String
buildString (NExpApp _ _ (l@(NExpLiteral _ _):r) _)
  = (:) (getCharFromLit l)
        (if (isEmptyList (mHead "buildString 2" r))
          then []
          else buildString (mHead "buildString 3" r))
buildString (NExpApp _ (NExpApp _ _ (l@(NExpLiteral _ _):r1) _) r2 _)
  = (:) (getCharFromLit l)
        (if (isEmptyList (mHead "buildString 2" (r1 ++ r2)))
          then []
          else buildString (mHead "buildString 3" (r1 ++ r2)))


isEmptyList :: NodeExp -> Bool
isEmptyList (NExpConstructor _ (Qualified "Prelude" "[]") _) = True
isEmptyList exp = False

getCharFromLit :: NodeExp -> Char
getCharFromLit (NExpLiteral node charList)
  = (mHead "getCharFromLit 2" (tail charList))

mHead r l = seq (if null l then unsafePerformIO $ putStrLn r else ())
                head l