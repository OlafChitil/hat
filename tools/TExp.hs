module TExp
  ( TExp
  , linearise
  ) where

import SExp

-- The TExp datatype is a tokenisation of the SExp type
data TExp
  = TApp
  | TId QName
  | TLiteral String
  | TString String
  | TWithin
  | TLambda
  | TIf
  | TCase
  | TGuard
  | TCut
  | TUnevaluated
  | TInterrupted
  | TBottom
  | TCycle
  | TEquation
  | TFieldExpr
-- ...with added bracketing to disambiguate arguments
  | TOpen
  | TClose
  | TRHS
  deriving Eq

-- linearise converts the SExp tree to a stream of tokens, by
-- pre-order traversal
linearise :: SExp a -> [TExp]
linearise (SEquation x lhs rhs) = TEquation: linearise lhs
                                        ++ TRHS: linearise rhs
linearise (SApp _ es)      = TOpen: TApp: concatMap linearise es ++ [TClose]
linearise (SId _ s _)      = [TId s]
linearise (SLiteral _ s)   = [TLiteral s]
linearise (SString _ s _)  = [TString s]
linearise (SWithin _ es)   = TOpen: TWithin: concatMap linearise es ++ [TClose]
linearise (SLambda _)      = [TLambda]
linearise (SIf _ e _)      = TIf: linearise e
linearise (SCase _ e _)    = TCase: linearise e
linearise (SGuard _ e _)   = TGuard: linearise e
linearise (SCut _)         = [TCut]
linearise (SUnevaluated _) = [TUnevaluated]
linearise (SInterrupted _) = [TInterrupted]
linearise (SBottom _)      = [TBottom]
linearise (SDoStmt _ _ e p)= TOpen: TApp: TLambda: linearise e ++ [TClose]
linearise (SCycle _ _ e)   = TCycle: linearise e
linearise (SFieldExpr _ e fs vs) = TOpen: TFieldExpr: linearise e
                                   ++ concatMap linearise vs ++ [TClose]

