-- Environment for tracing transformation.
-- Holds information about all identifiers (names) in scope.

module Environment
  (Environment
  ,TySynBody(..)
  ,arity,isLambdaBound,isTracedQName, mutateLetBound,fixPriority
  ,isExpandableTypeSynonym,typeSynonymBody
  ) where

import Language.Haskell.Exts.Annotated (QName, Name)


newtype Environment = Env Int

data TySynBody = THelper Int | TVar Int | TFun Int | TApp TySynBody TySynBody


arity :: Environment -> QName l -> Maybe Int
arity = undefined

typeSynonymBody :: Environment -> QName l -> Maybe TySynBody
typeSynonymBody = undefined

isLambdaBound :: Environment -> QName l -> Bool
isLambdaBound = undefined

isTracedQName :: Environment -> QName l -> Bool
isTracedQName = undefined

isExpandableTypeSynonym :: Environment -> QName l -> Bool
isExpandableTypeSynonym = undefined

mutateLetBound :: Environment -> Name l -> Environment
mutateLetBound = undefined

fixPriority :: Environment -> Name l -> Int
fixPriority = undefined