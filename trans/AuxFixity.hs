{- ---------------------------------------------------------------------------
Restructure expressions with infix operators according to associativity
and precedence.  This variant of the infix resolution code is only
used when performing the tracing transformation.  When doing ordinary
compilation, resolving infix expressions is part of the Rename phase.
-}

module AuxFixity(fixInfixList) where

import SysDeps(PackedString,packString,unpackPS)
import Extra(Pos(..),noPos,strPos,pair)
import Syntax
import SyntaxPos
import TokenId(TokenId(..), t_x, t_y, t_flip, t_minus, tnegate, visImport)
import AssocTree
--import PreImp
import AuxTypes
import MkSyntax(mkAppExp)


{-
-- Main function of the module.
-}
fixInfixList :: Environment -> [Exp TokenId] -> Exp TokenId

fixInfixList env [] = error "I: fixInfix []"
fixInfixList env ees@(ExpVarOp pos op:es) =
  let fix = lookupFix env op
      exp' = reorder env es
      exp  = invertCheck pos op fix env exp'
--in ExpLambda pos [varx,vary] 
--                  (ExpApplication pos [ExpVar pos op,vary,varx])
--  where
--  varx = ExpVar pos t_x
--  vary = ExpVar pos t_y
  in if op == t_minus
     then reorder env ees
     else (mkAppExp [ExpVar noPos t_flip, ExpVar pos op, exp])
             -- desugaring with flip better than lambda for reading a trace
fixInfixList env ees@(ExpConOp pos op:es) =
  let fix = lookupFix env op
  in case fix of
	(Pre a,l) -> reorder env ees
	_ -> let exp' = reorder env es
                 exp  = invertCheck pos op fix env exp'
--           in ExpLambda pos [varx,vary] 
--                  (ExpApplication pos [ExpVar pos op,vary,varx])
--  where
--  varx = ExpVar pos t_x
--  vary = ExpVar pos t_y
             in mkAppExp [ExpVar noPos t_flip, ExpCon pos op, exp]
             -- desugaring with flip better than lambda for reading a trace
fixInfixList env ees =
  case last ees of
    ExpConOp pos op -> let fix  = lookupFix env op
			   exp' = reorder env (init ees)
                           exp  = invertCheck pos op fix env exp'
                       in (mkAppExp [ExpCon pos op,exp])
    ExpVarOp pos op -> let fix  = lookupFix env op
			   exp' = reorder env (init ees)
                           exp  = invertCheck pos op fix env exp'
                       in (mkAppExp [ExpVar pos op,exp])
    _ -> reorder env ees



-- the primary function that re-orders an infix list into a single expression
reorder :: Environment -> [Exp TokenId] -> Exp TokenId
reorder env es = getExp env [] [] es


-- getExp and getOp together `parse' the list of expressions into a
-- single tree-shaped expression.  We expect the list to take the form
-- [exp,op,exp,op,exp,op...] i.e. exps and ops alternate.  Of course,
-- there are exceptions: where the whole expr is a left or right section
-- of an operator; and where a prefix operator starts the show.

getExp :: Environment			-- environment (contains fixity info)
	-> [((Fixity,Int),(Exp TokenId,Int))] -- stack of operators already seen
	-> [Exp TokenId]		-- stack of outstanding non-op exprs
	-> [Exp TokenId]		-- input list of exprs
	-> Exp TokenId			-- re-combined output

getExp env ops exps (e:es) =
  case e of
    ExpConOp pos o ->
      let fix = lookupFix env o in
        case fix of
	  (Pre a,l) -> getExp env (stackPrefix fix (ExpCon pos o):ops) exps es
    ExpVarOp pos o ->
      let fix = lookupFix env o in
        case fix of
	  (Pre a,l) -> getExp env (stackPrefix fix (ExpVar pos o):ops) exps es
	  _ | o==t_minus
                    -> getExp env (stackPrefix (Pre "negate",6) (ExpVar pos o):ops) exps es
    _ -> getOp env ops (e:exps) es
getExp env ops [] [] =
   error ("Problem with infix section at unknown location.")
getExp env ops (e:es) [] =
   error ("Problem with infix section at "++strPos (getPos e))


getOp :: Environment			-- environment (contains fixity info)
	-> [((Fixity,Int),(Exp TokenId,Int))]	-- stack of operators
	-> [Exp TokenId]		-- stack of outstanding non-op exprs
	-> [Exp TokenId]		-- input list of exprs
	-> Exp TokenId			-- recombined output

getOp env ops exps [] = finish ops exps
getOp env ops exps ees@(ExpConOp pos op:es) =
  case harder pos ops op env of
    Just (o,ops) -> getOp env ops (rebuild o exps) ees
    Nothing      -> let fop = stackInfix env (ExpCon pos op)
                    in getExp env (fop:ops) exps es
getOp env ops exps ees@(ExpVarOp pos op:es) =
  case harder pos ops op env of
    Just  (o,ops) -> getOp env ops (rebuild o exps) ees
    Nothing       -> let fop = stackInfix env (ExpVar pos op)
                     in getExp env (fop:ops) exps es
getOp env ops exps (e:es) =
   error ("Need infix operator at " ++ strPos (getPos e))


stackInfix :: Environment -> Exp TokenId -> ((Fixity,Int),(Exp TokenId,Int))
stackInfix env op@(ExpVar _ o) = (lookupFix env o, (op,2::Int))
stackInfix env op@(ExpCon _ o) = (lookupFix env o, (op,2::Int))

stackPrefix fix op = (fix,(op,1::Int))
 

-- harder decides whether a new operator has lower precedence than
-- the top of the operator stack.  If so, we can pop the stack and
-- rebuild the accumulated expression so far before continuing.
-- If however it has higher precedence, then we must push the new
-- operator onto the stack and keep going.  If it has equal precedence,
-- then left/right associativity must be taken into account.

harder :: Pos				-- position
	-> [((Fixity,Int),(b,c))]	-- stack of operators
	-> TokenId			-- operator name
	-> Environment			-- environment holding fixity info
	-> Maybe (((Fixity,Int),(b,c)),[((Fixity,Int),(b,c))])
harder pos [] op' env = Nothing
harder pos (ipop@((inf,pri),(_,_)):ops) op' env =
  let (inf',pri') = lookupFix env op' in
  if pri > pri' then
    Just (ipop,ops)
  else if pri == pri' then
    case inf of
      Def   -> Just (ipop,ops)
      L     -> Just (ipop,ops)
      Pre _ -> Just (ipop,ops)
      R     -> Nothing
      None  -> error ("Infix operator "++show op'++" at "++strPos pos
                      ++" is non-associative.")
  else Nothing



-- finish transforms the two stacks (operators + exprs) into the final
-- recombined expression.
finish :: [((Fixity,c),(Exp TokenId,Int))]	-- stack of operators
          -> [Exp TokenId]			-- stack of non-op exprs
          -> Exp TokenId			-- output expr
finish [] []  = error "AuxFixity.finish: empty" 
finish [] [e] = e
finish [] _   = error "AuxFixity.finish: multiple expression"
finish (o:ops) es = finish ops (rebuild o es)


-- rebuild the expression stack, by combining the top two items with
-- the given operator.  Precondition: the operator must have higher
-- precedence than any operators between the exprs lower in the stack.

rebuild :: ((Fixity,c),(Exp TokenId,Int))	-- operator
           -> [Exp TokenId]			-- stack of expressions
           -> [Exp TokenId]			-- juggled the stack
rebuild (_,(op,2)) (e1:e2:es) =
        mkAppExp [op,e2,e1]:es
rebuild ((Pre fun,_) ,(op,_)) (e1:es) =
        mkAppExp [ExpVar (getPos op) (visImport fun),e1]:es
rebuild (_,(op,n)) es =
        error ("Not enough arguments at " ++ strPos (getPos op))


-- discover fixity information from the .hx file info.
lookupFix :: Environment -> TokenId -> (Fixity,Int)
lookupFix (env,identMap) op =
    case lookupAT env (useIdentMap identMap op) of
	Just info -> (fixity info, priority info)
	Nothing   -> error ("No auxinfo for operator "++show op)


leftFixity :: Fixity -> Bool
leftFixity L       = True
leftFixity Def     = True
leftFixity (Pre _) = True
leftFixity _       = False     		--- !!! Cheating Infix is InfixR  (??)


-- 'invertCheck' checks for priority inversion in an operator section.
invertCheck :: Show a => Pos -> a -> (b,Int) -> Environment
		-> Exp TokenId -> Exp TokenId
invertCheck pos1 op1 (fix1,pri1) env exp =
  case exp of
    ExpApplication _ (ExpVar pos2 op2: es) -> check pos2 op2
    ExpApplication _ (ExpCon pos2 op2: es) -> check pos2 op2
    _ -> exp
  where
    check pos2 op2 =
      let (fix2,pri2) = lookupFix env op2
      in if pri2 < pri1 then
        error ("Fixity problem:\n  "
              ++show op1++" used at "++strPos pos1++" has precedence "
              ++show pri1++",\n  "
              ++show op2++" used at "++strPos pos2++" has precedence "
              ++show pri2++".\n  "
              ++"The partially applied operator "++show op1
              ++" should have lower precedence\n  "
              ++"than the fully-applied operator "
              ++show op2++" used inside the section.\n")
      else exp

{- --------------------------------------------------------------------------}
