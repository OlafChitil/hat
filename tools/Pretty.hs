module Pretty (PrettyOption(..), graphForNode, makeGraph) where

-- #if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 604 ) \
--    || ( defined(__NHC__) && __NHC__ < 118 )
-- #define empty emptySet
-- #define toList setToList
-- #define insert addToSet
-- #define member elementOf
-- #endif

import LowLevel      (FileNode(..),nil,getParentNode,getSrcRef)
import CommonUI      (Options(..),Keep(..))
import NodeExp       (NodeExp(..),fullEval,flatEval,branches,result
                     ,removeResultCycles,removeNonResultCycles
                     ,nodeExp2SExp,getNode,CondType(..),getFirstCaredNode
                     ,flatEvalText,nodeExpForNode,fullEvalText,(===))
import SExp          (QName(..),showQN,prettySExp)
import qualified Data.Set as Set (empty, toList, insert, Set(..),member)
import ADT           (ADT(..),detectCycles,trustModule,trustIO,trustConstant
                     ,foldHiddens,trustUnevaluated,displayTrees)
import EDT           (buildEDT)
import SrcRef        (SrcRef(..), readSrcRef)

import Data.Char     (chr)
import Data.List     (sort)

import System.IO.Unsafe (unsafePerformIO)

-- #if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 604 ) \
--     || ( defined(__NHC__) && __NHC__ < 118 )
-- addToSet :: Ord a => a -> Set.Set a -> Set.Set a
-- addToSet = flip Set.addToSet
-- setToList = Set.setToList
-- #else
addToSet :: Ord a => a -> Set.Set a -> Set.Set a
addToSet = Set.insert
setToList = Set.toList
-- #endif

data PrettyOption = ShowDecOffsets
                  | ShowHexOffsets
                  | ShowParents
                  | MkEDT
                  | MkFDT
                  | ShowSrcPoses
                  | ShowVersion
                  | ShowModule String
                  deriving Eq

parents :: [PrettyOption] -> Bool
parents = elem ShowParents

makeGraph :: NodeExp -> [PrettyOption] -> String
makeGraph n o = if MkEDT `elem` o
                  then
                    "digraph edt\n" ++
                    "    {\n" ++
                    "    ordering=out; rankstep=0.1;\n" ++
                    (indent 4 (graphForEDT n o)) ++
                    "    }\n"
                  else
                    "digraph trace\n" ++
                    "    {\n" ++
                    "    ordering=out; rankstep=0.1;\n" ++
                    (indent 4 (graphForNode n o)) ++
                    "    }\n"

indent :: Int -> String -> String
indent n = unlines . (map ((++) (take n (repeat ' ')))) . lines

graphForNode :: NodeExp -> [PrettyOption] -> String
graphForNode x o = (dotNodes y o) ++ (dotEdges y o)
                   where
                     y = moveToSet (removeNonResultCycles (removeResultCycles x)) Set.empty

graphForEDT :: NodeExp -> [PrettyOption] -> String
graphForEDT x o =
  (concat (map (flip dotADTNodes o) y)) ++
  (concat (map (flip dotADTEdges o) y)) 
  where
    y = (  trustConstant (Plain "otherwise")
         . trustModule "Prelude"
         . detectCycles
         . trustUnevaluated
         . trustIO
         . foldHiddens
         . buildEDT) $ removeNonResultCycles $ removeResultCycles x

moveToSet :: NodeExp -> Set.Set NodeExp -> Set.Set NodeExp
moveToSet n s
  | n `Set.member` s = s
  | otherwise = addToSet n $ foldr moveToSet s $ branches n

{-moveToSet' :: NodeExp -> Set.Set NodeExp -> Set.Set NodeExp
moveToSet' node@(NExpApp _ fun args res) set
  | (isVCycleOrApp res) && (getNode node) == (getNode res) && isId fun
    = if addToSet node (foldr moveToSet set args)
  | isId fun
    = addToSet node (moveToSet res (foldr moveToSet set args))
  | otherwise
    = addToSet node (moveToSet fun
                      (moveToSet res
                        (foldr moveToSet set args)))
moveToSet' node@(NExpConstUse _ def res) set
  = addToSet node (moveToSet res set)
moveToSet' node@(NExpConstDef _ def res) set
  = addToSet node (moveToSet res set)
moveToSet' node@(NExpCond _ _ cnd res) set
  = addToSet node (moveToSet cnd (moveToSet res set))
moveToSet' node@(NExpFieldExp _ field mappings) set
  = addToSet node (moveToSet field (foldr (moveToSet . snd) set mappings))
moveToSet' node@(NExpProjection _ res) set
  = addToSet node (moveToSet res set)
moveToSet' node@(NExpForward _ res) set
  = moveToSet res set
moveToSet' node@(NExpHidden _ children res) set
  = addToSet node (moveToSet res (foldr moveToSet set children))
moveToSet' node set
  = addToSet node set-}

isId :: NodeExp -> Bool
isId (NExpForward _ r) = isId r
isId (NExpIdentifier _ _ _) = True
isId _ = False

isVCycleOrApp (NExpResultCycle _) = True
isVCycleOrApp (NExpApp _ _ _ _) = True
isVCycleOrApp _ = False

dotNodes :: Set.Set NodeExp -> [PrettyOption] -> String
dotNodes s o
  = {-concatMap makeNode (setToList s)-}
    concatMap (\s -> "{rank=same; " ++ concatMap makeNode s ++ "}\n") (groupSets s)
    where
      groupSets :: Set.Set NodeExp -> [[NodeExp]]
      groupSets s = groupSets' $ sort $ setToList s
      
      groupSets' :: [NodeExp] -> [[NodeExp]]
      groupSets' [] = []
      groupSets' (x:xs) =
        (collected:groupSets' (filter (not . (`elem` collected)) xs))
        where
          collected = collectRs x []
      
      collectRs n v = if n `elem` v then []
                                    else n:collectRs (result n) (n:v)
            
      makeNode :: NodeExp -> String
      makeNode node@(NExpApp id fun args res)
        | (isVCycleOrApp res) && (getNode node) == (getNode res) && isId fun
          = mkNode id "ValApp" $ quote (flatEvalText 100 fun)
        | isId fun
          = let pos = getSrcRef id
            in mkNode id "Application" $ quote (flatEvalText 100 fun ++ if ShowSrcPoses `elem` o && pos /= LowLevel.nil then " - " ++ (formatSrcRef $ readSrcRef $ pos) else "")
        | otherwise
          = mkNode id "Application" ""
      makeNode node@(NExpConstUse id def res)
        = mkNode id "Constant Use" $ quote (showQN False def ++  if ShowSrcPoses `elem` o && pos /= LowLevel.nil then " - " ++ (formatSrcRef $ readSrcRef $ pos) else "")
          where pos = getSrcRef id
      makeNode node@(NExpConstDef id def res)
        = mkNode id "Constant Def" $ quote $ showQN False def
      makeNode node@(NExpIdentifier id _ _)
        = mkNode id "Identifier" $ quote (flatEvalText 100 node ++ if ShowSrcPoses `elem` o && pos /= LowLevel.nil then " - " ++ (formatSrcRef $ readSrcRef $ pos) else "")
          where pos = getSrcRef id
      makeNode node@(NExpLiteral id name)
        = mkNode id "Literal" $ quote (name ++ if ShowSrcPoses `elem` o && pos /= LowLevel.nil then " - " ++ (formatSrcRef $ readSrcRef $ pos) else "")
          where pos = getSrcRef id
      makeNode node@(NExpHidden id children res)
        = mkNode id "Hidden" ""
      makeNode node@(NExpProjection id res)
        = mkNode id "Projection" $ quote $ if ShowSrcPoses `elem` o && pos /= LowLevel.nil then formatSrcRef $ readSrcRef $ pos else ""
          where pos = getSrcRef id
      makeNode node@(NExpCond id gType cnd res)
        | gType == IfCond    = mkNode id "If" ""
        | gType == CaseCond  = mkNode id "Case" ""
        | gType == GuardCond = mkNode id "Guard" ""
      makeNode node@(NExpFieldExp id field mappings)
        = mkNode id "Field Expression" $ quote $ if ShowSrcPoses `elem` o then formatSrcRef $ readSrcRef $ getSrcRef id else ""
      makeNode node@(NExpInterrupt)
        = mkNode nil "^C" ""
      makeNode node@(NExpUneval)
        = mkNode nil "Unevaluated" ""
      makeNode node@(NExpBottom)
        = mkNode nil "_|_" ""
      makeNode node@(NExpNotShown)
        = mkNode nil "Not Shown" ""
      makeNode _ = ""
      
      mkNode :: FileNode -> String -> String -> String
      mkNode id lab ""
        | lab == "^C"          = insertOffset id
                                              "int [shape=diamond, label=\""
                                              "^C\"];\n"
        | lab == "_|_"         = insertOffset id
                                              "bot [shape=diamond, label=\""
                                              "_|_\"];\n"
        | lab == "Not Shown"   = insertOffset id
                                              "ns [shape=diamond, label=\""
                                              "Not Shown\"];\n"
        | otherwise
          = insertOffset id
                         ("x" ++ (show id)
                          ++ " [shape=box, label=\"")
                         (lab
                          ++ "\", fontname=\"Monaco\", fontsize=10];\n")
      mkNode id lab desc
        | lab == "ValApp" || lab == "Literal" || lab == "Identifier"
          = insertOffset id
                         ("x" ++ (show id)
                          ++ " [shape=ellipse, label=\"")
                         (desc
                          ++ "\", fontname=\"Monaco\", fontsize=10];\n")
        | otherwise
          = insertOffset id
                         ("x" ++ (show id)
                          ++ " [shape=record, label=\"{")
                         (lab
                          ++ " | "
                          ++ desc
                          ++ "}\", fontname=\"Monaco\", fontsize=10];\n")
      
      insertOffset :: FileNode -> String -> String -> String
      insertOffset node@(FileNode offset) start end
        = start
          ++ (if ShowDecOffsets `elem` o then show offset ++ " "
              else "")
          ++ (if ShowHexOffsets `elem` o then "0x" ++ show node ++ " "
              else "")
          ++ end
      
      writeRank :: FileNode -> String
      writeRank (FileNode x) = (show x)

dotADTNodes :: ADT -> [PrettyOption] -> String
dotADTNodes adt@(Branch _ node _ others) o
  = makeNode node ++ (concat $ map ((flip dotADTNodes) o) others)
    where
      makeNode :: NodeExp -> String
      makeNode node@(NExpApp id fun args res)
        = mkNode id "" $ filter (/= '\n') $ quote $ (flatEvalText 100 node ++ " ~> " ++ fullEvalText 100 node)
      makeNode node@(NExpConstUse id def res)
        = mkNode id "" $ filter (/= '\n') $ quote $ (showQN False def ++ " ~> " ++ fullEvalText 100 node)
      makeNode node@(NExpConstDef id def res)
        = mkNode id "" $ filter (/= '\n') $ quote $ (showQN False def ++ " ~> " ++ fullEvalText 100 node)
      makeNode node@(NExpInterrupt)
        = mkNode nil "^C" ""
      makeNode node@(NExpUneval)
        = mkNode nil "Unevaluated" ""
      makeNode node@(NExpBottom)
        = mkNode nil "_|_" ""
      makeNode node@(NExpNotShown)
        = mkNode nil "Not Shown" ""
      makeNode _ = ""
      
      mkNode :: FileNode -> String -> String -> String
      mkNode id lab ""
        | lab == "^C"          = insertOffset id
                                              "int [shape=diamond, label=\""
                                              "^C\"];\n"
        | lab == "_|_"         = insertOffset id
                                              "bot [shape=diamond, label=\""
                                              "_|_\"];\n"
        | lab == "Not Shown"   = insertOffset id
                                              "ns [shape=diamond, label=\""
                                              "Not Shown\"];\n"
        | otherwise
          = insertOffset id
                         ("x" ++ (show id)
                          ++ " [shape=box, label=\"")
                         (lab
                          ++ "\", fontname=\"Monaco\", fontsize=10];\n")
      mkNode id lab desc
        = insertOffset id
                       ("x" ++ (show id)
                       ++ " [shape=box, label=\"")
                       (desc
                       ++ "\", fontname=\"Monaco\", fontsize=10];\n")
      
      insertOffset :: FileNode -> String -> String -> String
      insertOffset node@(FileNode offset) start end
        = start
          ++ (if ShowDecOffsets `elem` o then show offset ++ " "
              else "")
          ++ (if ShowHexOffsets `elem` o then "0x" ++ show node ++ " "
              else "")
          ++ end
dotADTNodes adt@(Cycle _ nodes) o = ""

formatSrcRef :: SrcRef -> String
formatSrcRef SrcRef {filename = f, line = sl, column = sc, lineend = el, columnend = ec} =
  f ++ ":" ++ show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec

dotEdges :: Set.Set NodeExp -> [PrettyOption] -> String
dotEdges s o
  = makeEdges (setToList s)
    where
      makeEdges :: [NodeExp] -> String
      makeEdges [] = ""
      makeEdges (x:xs) = makeEdge x ++ makeEdges xs
      
      makeEdge :: NodeExp -> String
      makeEdge node@(NExpApp id fun args res)
        | (isVCycleOrApp res) && (getNode node) == (getNode res) && isId fun
          =    (foldr ((++) . ((flip (mkEdge id)) "Arg")) "" args)
            ++ if (parents o) then (mkEdge (getParentNode id)
                                           node
                                           "Par")
                              else ""
        | isId fun
          =    (foldr ((++) . ((flip (mkEdge id)) "Arg")) "" args)
            ++ mkEdge id res "Res"
            ++ if (parents o) then (mkEdge (getParentNode id)
                                           node
                                           "Par")
                              else ""
        | otherwise
          =    mkEdge id fun "Fun"
            ++ (foldr ((++) . ((flip (mkEdge id)) "Arg")) "" args)
            ++ mkEdge id res "Res"
            ++ if (parents o) then (mkEdge (getParentNode id)
                                           node
                                           "Par")
                              else ""
      makeEdge node@(NExpConstUse id def res)
        =    mkEdge id res "Res"
          ++ if (parents o) then (mkEdge (getParentNode id)
                                         node
                                         "Par")
                            else ""
      makeEdge node@(NExpConstDef id def res)
        =    mkEdge id res "Res"
          ++ if (parents o) then (mkEdge (getParentNode id)
                                         node
                                         "Par")
                            else ""
      makeEdge node@(NExpCond id gType cnd res)
        =    mkEdge id cnd "Cond"
          ++ mkEdge id res "Res"
      {-makeEdge node@(NExpFieldExp id field mappings)
      -}
      makeEdge node@(NExpHidden id children res)
        =    (foldr ((++) . ((flip (mkEdge id)) "Chld")) "" children)
          ++ mkEdge id res "Res"
      makeEdge node@(NExpProjection id res)
        = mkEdge id res "Res"
      makeEdge node = ""

      mkEdge :: Show a => a -> NodeExp -> String -> String
      mkEdge src dst lbl
        =    "x" ++ (show src)
          ++ " -> "
          ++ "x" ++ (show (getNode (findChild dst)))
          ++ " ["
          ++ lineStyle lbl
          ++ "];\n"
          where
            lineStyle :: String -> String
            lineStyle "Fun"   = "style=dashed"
            lineStyle "Const" = "style=dashed"
            lineStyle "Res"   = "style=bold"
            lineStyle "Par"   = "style=dotted, dir=back"
            lineStyle "Arg"   = "arrowhead=inv"
            lineStyle "Cond"  = ""
            lineStyle "Chld"  = "arrowhead=inv"
            lineStyle x       = "label=\"" ++ x ++ "\""

dotADTEdges :: ADT -> [PrettyOption] -> String
dotADTEdges adt opts =
  concat $ zipWith mkADTEdge starts ends
  where
    (starts,ends) = unzip $ setToList $ edgeSet
    edgeSet = adtEdgeSet adt Set.empty

foldrM1 :: (a -> IO ()) -> [a] -> IO ()
foldrM1 f = foldr (\x y -> y >> f x) (return ())

adtEdgeSet :: ADT -> Set.Set (ADT,ADT) -> Set.Set (ADT,ADT)
adtEdgeSet adt@(Branch _ exp _ others) s
  = foldr (\x y -> addToSet (adt,x) y) addedSet others
    where
      addedSet = doSetAdds others
      doSetAdds [] = s
      doSetAdds (x:xs) = adtEdgeSet x (doSetAdds xs)
adtEdgeSet adt@(Cycle _ adts) s
  = foldr (\x y -> addToSet (adt,x) y) s adts

mkADTEdge :: ADT -> ADT -> String
mkADTEdge src@(Branch _ exp _ _) dst@(Branch _ exp' _ _)
  =    "x" ++ (show (getNode exp))
    ++ " -> "
    ++ "x" ++ (show (getNode exp')) ++ "\n"
mkADTEdge src@(Branch _ exp _ _) dst@(Cycle tag exps)
  = concatMap (mkADTEdge src) exps
mkADTEdge _ _ = ""

findChild :: NodeExp -> NodeExp
findChild (NExpForward _ res) = findChild res
-- findChild (NExpHidden _ children res) = findChild res
-- findChild (NExpProjection _ res) = findChild res
findChild x = x

bracket :: String -> String
bracket x = '(' : (x ++ ")")

quote :: String -> String
quote [] = []
quote (x:xs)
  | x == '{'   = "\\{" ++ quote xs
  | x == '}'   = "\\}" ++ quote xs
  | x == '"'   = "\\\"" ++ quote xs
  | x == '\\'  = "\\\\" ++ quote xs
  | x == '<'   = "\\<" ++ quote xs
  | x == '>'   = "\\>" ++ quote xs
  | x == '\27' = quote $ drop 1 $ dropWhile (/='m') xs
  | otherwise  = x : (quote xs)
