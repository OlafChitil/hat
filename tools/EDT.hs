{-- | This module provides facilities for building Evaluation Dependancy Trees, 
    | trusting of modules, functions and applications, displaying trees, and
    | finding parts of the tree.
    | --}
module EDT (buildEDT)
           where

import NodeExp (NodeExp(..),children,flatEvalText,fullEvalText)
import ADT     (ADT(..))
import Detect  (questionText)

-- | Creates an evaluation dependancy tree
buildEDT :: NodeExp -- ^ The trace to gather an EDT from -
                    -- ^ in the form of a NodeExp
         -> [ADT]
buildEDT = buildEDT' [1]
buildEDT' :: [Int] -> NodeExp -> [ADT]
buildEDT' t exp@(NExpApp n _ _ _) =
  [Branch t exp (edtQuestion exp) (concatZipWith (\e nt -> buildEDT' (nt:t) e)
                                                 (children exp)
                                                 [1..])]
buildEDT' t exp@(NExpConstUse n _ _) =
  [Branch t exp (edtQuestion exp) (concatZipWith (\e nt -> buildEDT' (nt:t) e)
                                                 (children exp)
                                                 [1..])]
buildEDT' t exp@(NExpConstDef n _ _) = 
  [Branch t exp (edtQuestion exp) (concatZipWith (\e nt -> buildEDT' (nt:t) e)
                                                 (children exp)
                                                 [1..])]
buildEDT' t exp@(NExpCond n _ _ r) = 
  buildEDT' t r
buildEDT' t exp@(NExpProjection n r) = 
  buildEDT' t r
buildEDT' t exp@(NExpForward n r) = 
  buildEDT' t r
buildEDT' t exp@(NExpHidden n _ _) =
  (concatZipWith (\e nt -> buildEDT' (nt:t) e)
                               (children exp)
                               [1..])
buildEDT' _ _ = []

concatZipWith f l1 l2= concat (zipWith f l1 l2)

edtQuestion :: NodeExp -> Int -> String
edtQuestion e w = questionText w (flatEvalText w) (fullEvalText w) e
