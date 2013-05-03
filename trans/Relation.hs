-- ------------------------------------------------------------------------------------
-- Relations and their functions
--
-- This interface is based on 
-- Diatchki, Jones, Hallgren: "A Formal Specificationof the Haskell 98 Module System"
-- Haskell 2002, ACM
--
-- The type representation is different.
-- It is chosen to be efficient for frequent lookups and also insertions.

module Relation
  (Relation
  ,listToRelation,relationToList,emptyRelation,restrictDom,restrictRng
  ,dom,rng,mapDom,mapRng,intersectRelation,unionRelations,minusRelation
  ,partitionDom,applyRelation
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- The set is always non-empty.
type Relation a b = Map.Map a (Set.Set b)

-- pre-condition: input list is in ascending order in first component
listToRelation :: (Ord a, Ord b) => [(a,b)] -> Relation a b
listToRelation xs = 
  Map.fromAscListWith (Set.union) (map (\(x,y) -> (x,Set.singleton y)) xs)

-- post-condition: output list is in ascending order in first component
relationToList :: Relation a b -> [(a,b)]
relationToList r = 
  concatMap (\(x,ys) -> map (\y -> (x,y)) (Set.toAscList ys)) (Map.toAscList r)

emptyRelation :: Relation a b
emptyRelation = Map.empty

restrictDom :: (Ord a, Ord b) => (a -> Bool) -> Relation a b -> Relation a b
restrictDom p r = Map.filterWithKey (\a _ -> p a) r

restrictRng :: (Ord a, Ord b) => (b -> Bool) -> Relation a b -> Relation a b
restrictRng p = Map.filterWithKey (\_ bs -> Set.null bs) . Map.map (Set.filter p)

dom :: Ord a => Relation a b -> Set.Set a
dom r = Map.keysSet r

rng :: Ord b => Relation a b -> Set.Set b
rng r = Set.unions (Map.elems r)

mapDom :: (Ord b, Ord c) => (a -> c) -> Relation a b -> Relation c b
mapDom f = Map.mapKeysWith (Set.union) f 

mapRng :: (Ord a, Ord b, Ord c) => (b -> c) -> Relation a b -> Relation a c
mapRng f = Map.map (Set.map f)

intersectRelation :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersectRelation = Map.intersectionWith (Set.intersection)

unionRelations :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unionRelations rs = Map.unionsWith (Set.union) rs

minusRelation :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
minusRelation r1 r2 = Map.differenceWith subtract r1 r2
  where
  subtract s1 s2 = let s = Set.difference s1 s2 
                   in if Set.null s then Nothing else Just s

partitionDom :: Ord a => (a -> Bool) -> Relation a b -> (Relation a b, Relation a b)
partitionDom p = Map.partitionWithKey (\a bs -> p a)

applyRelation :: (Ord a, Ord b) => Relation a b -> a -> Set.Set b
applyRelation r a = r Map.! a
