-----------------------------------------------------------------------------
-- |
-- Module      :  Trie
-- Copyright   :  (c) Malcolm Wallace 2002
-- License     :  Hat (or GPL, if used outside Hat)
-- 
-- Maintainer  :  Malcolm.Wallace@cs.york.ac.uk
-- Stability   :  stable
-- Portability :  portable
--
--  A simple searchable Trie.
--
-----------------------------------------------------------------------------
module Trie
  ( Trie                -- type exported abstract
  , emptyTrie           -- :: Trie a
  , Search(..)          -- data Search a = Exists | New a
  , match               -- :: [a] -> Trie a -> Search (Trie a)
  ) where

-- | A Trie is a lookup structure for strings of tokens.
-- * Any path through the Trie that terminates in a Found constructor
--   indicates that the string represented by that path is stored.
-- * A path that goes through a FoundAnd constructor indicates that
--   the prefix (up to FoundAnd) is definitely stored, and possibly
--   its continuation also.
-- * Any path or partial path that does not end with either a Found
--   or FoundAnd, means that the corresponding string is not stored.

data Trie a = Trie [(a,Trie a)] | Found | FoundAnd [(a,Trie a)]
                deriving Show
data Search a = Exists | New {fromNew::a}
                deriving Show

emptyTrie :: Trie a
emptyTrie  = Trie []

-- | `match' takes a string and a trie and tries to find the string in the trie.
--   If the string is present in the Trie, we return Exists, if not, we return
--   a New trie with the string added to it.
match :: Eq a => [a] -> Trie a -> Search (Trie a)

match [t]    (Trie xs) =
  case lookup t xs of
    Nothing           -> New (Trie ((t,Found):xs))
    Just Found        -> Exists
    Just (FoundAnd _) -> Exists
    Just (Trie ys)    -> New (Trie (replace (t,FoundAnd ys) xs))

match (t:ts) (Trie xs) =
  case lookup t xs of
    Nothing    -> New (Trie ((t, ripple ts):xs))
    Just Found -> New (Trie (replace (t, foundAnd (ripple ts)) xs))
    Just y     -> case match ts y of
                      Exists -> Exists
                      New tr -> New (Trie (replace (t, tr) xs))

match [t]    (FoundAnd xs) =
  case lookup t xs of
    Nothing           -> New (FoundAnd ((t,Found):xs))
    Just Found        -> Exists
    Just (FoundAnd _) -> Exists
    Just (Trie ys)    -> New (FoundAnd (replace (t,FoundAnd ys) xs))

match (t:ts) (FoundAnd xs) =
  case lookup t xs of
    Nothing    -> New (FoundAnd ((t,ripple ts):xs))
    Just Found -> New (FoundAnd (replace (t, foundAnd (ripple ts)) xs))
    Just y     -> case match ts y of
                      Exists -> Exists
                      New tr -> New (FoundAnd (replace (t, tr) xs))


---------------------------------------------------------------------------
-- In the Trie, when a new path is inserted, it replaces the old one.
replace :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
replace (x,y) ((a,b):abs) | x==a      = (x,y): abs
                          | otherwise = (a,b): replace (x,y) abs

-- To build a single path from an empty Trie, we just ripple the string of
-- tokens down.
ripple :: [a] -> Trie a
ripple []     = Found
ripple (x:xs) = Trie [(x,ripple xs)]

-- Convert a Trie to a FoundAnd.
foundAnd :: Trie a -> Trie a
foundAnd (Trie xs) = FoundAnd xs

---------------------------------------------------------------------------
