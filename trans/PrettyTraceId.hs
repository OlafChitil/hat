{-
Convert abstract syntax tree (or parts of it) into 
a structured document for pretty printing.
-}

module PrettyTraceId
  ( module PrettySyntax
  , prettyPrintTraceId
  ) where 

import PrettyLib
import PrettySyntax
import Flags (Flags,sShowWidth,sShowQualified,sShowIndent)
import TraceId (TraceId,hasValueInfo,arity,isLambdaBound,tokenId)
import AuxTypes (AuxiliaryInfo) -- hbc's broken import mechanism needs this
import TokenId (TokenId(..),extractV,t_Arrow,t_List)
import Id (Id)
import SysDeps (unpackPS)


prettyPrintTraceId :: Flags -> (PPInfo TraceId -> a -> Doc) -> a -> String

prettyPrintTraceId flags pp =
  pretty (sShowWidth flags) . 
  pp PPInfo{withPositions = False
           ,indent = sShowIndent flags
           ,id2str = id2strTraceId
           ,tyVar2str = id2strTraceId -- show
           ,isFunctionArrow = (\tr-> tokenId tr == t_Arrow)
           ,isList = (\tr-> tokenId tr == t_List)
           ,maybeTuple = maybeTupleTraceId}
  where
  id2strTraceId t = ( if sShowQualified flags
                      then show (tokenId t)
                      else (reverse . unpackPS . extractV) (tokenId t)
                    ) ++
                    ( if hasValueInfo t 
                        then "{-"++ show (arity t) ++"/"++
                             ( if isLambdaBound t then "lam-}" else "let-}" )
                        else "")
  maybeTupleTraceId t = case tokenId t of
                          TupleId n -> Just n
                          _         -> Nothing


