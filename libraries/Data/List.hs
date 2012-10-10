-- Haskell 2010 module by re-exporting Haskell 98 module and adding a bit

module Data.List (  
    (++),  head,  last,  tail,  init,  null,  length,  map,  reverse,  
    intersperse,  intercalate,  transpose,  subsequences,  permutations,  
    foldl,  foldl',  foldl1,  foldl1',  foldr,  foldr1,  concat,  concatMap,  
    and,  or,  any,  all,  sum,  product,  maximum,  minimum,  scanl,  scanl1,  
    scanr,  scanr1,  mapAccumL,  mapAccumR,  iterate,  repeat,  replicate,  
    cycle,  unfoldr,  take,  drop,  splitAt,  takeWhile,  dropWhile,  span,  
    break,  stripPrefix,  group,  inits,  tails,  isPrefixOf,  isSuffixOf,  
    isInfixOf,  elem,  notElem,  lookup,  find,  filter,  partition,  (!!),  
    elemIndex,  elemIndices,  findIndex,  findIndices,  zip,  zip3,  zip4,  
    zip5,  zip6,  zip7,  zipWith,  zipWith3,  zipWith4,  zipWith5,  zipWith6,  
    zipWith7,  unzip,  unzip3,  unzip4,  unzip5,  unzip6,  unzip7,  lines,  
    words,  unlines,  unwords,  nub,  delete,  (\\),  union,  intersect,  sort,  
    insert,  nubBy,  deleteBy,  deleteFirstsBy,  unionBy,  intersectBy,  
    groupBy,  sortBy,  insertBy,  maximumBy,  minimumBy,  genericLength,  
    genericTake,  genericDrop,  genericSplitAt,  genericIndex,  genericReplicate  
  ) where

import List

-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
-- result.
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

-- | A strict version of 'foldl'.
foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = let a' = f a x in a' `seq` foldl' f a' xs

-- | A strict version of 'foldl1'
foldl1'                  :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs)         =  foldl' f x xs
foldl1' _ []             =  error "Prelude.foldl1': empty list"

-- | The 'stripPrefix' function drops the given prefix from a list.
-- It returns 'Nothing' if the list did not start with the prefix
-- given, or 'Just' the list after the prefix, if it does.
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix "foo" "foo" == Just ""
-- > stripPrefix "foo" "barfoo" == Nothing
-- > stripPrefix "foo" "barfoobaz" == Nothing
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- iff the first list is contained, wholly and intact,
-- anywhere within the second.
--
-- Example:
--
-- >isInfixOf "Haskell" "I really like Haskell." == True
-- >isInfixOf "Ial" "I really like Haskell." == False
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
