-- Haskell 2010 module just re-exports Haskell 98 module

module Data.Maybe (  
    Maybe(Nothing, Just),  maybe,  isJust,  isNothing,  fromJust,  fromMaybe,  
    listToMaybe,  maybeToList,  catMaybes,  mapMaybe  
  ) where

import Maybe
