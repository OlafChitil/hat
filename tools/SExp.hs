-- Pretty printing of an ART expression with a sub-expression highlit.
-- First convert a value (basically a FileNode) into an S-expression,
-- then convert the S-expression to a Doc for pretty printing.
module SExp
  ( SExp(..)
  , SFixity(..), transFixity -- used only in module Pattern
  , Label
  , QName(..), showQN
  , fileNode2SExp
  , optParens
  , schemeParens
  , notCondParens
  , sExp2Doc
  , arity, child, label, children, rebuild, parent, prune
  , funId, funLabel
  , prettyExpression
  , prettySExp
  , prettyEquation
  , prettyEquation2
  ) where

import LowLevel hiding    (nil)
import qualified LowLevel (nil)
import HighlightStyle     (Highlight(..),Colour(..))
import PrettyLibHighlight as Pretty
                          (Doc,text,(<>),delimiter,fdelimiter,nil,group,parens
                          ,groupNest,pretty,highlight)
import CommonUI           (Options(..))
import Data.Char          (isAlpha)
import Data.List          (unzip3,isPrefixOf)
import System.IO          (hPutStrLn,stderr)
import System.IO.Unsafe  (unsafePerformIO)
import Numeric            (showHex)

bold = highlight [Bold, Foreground Blue]

errorT  :: String -> a
errorT s = unsafePerformIO (do hPutStrLn stderr s; return (error ""))
debug  :: (Show a) => a -> String -> a
x `debug` s = unsafePerformIO (do hPutStrLn stderr (s++show x); return x)

data QName
  = Plain String
  | Qualified String String
  deriving Show

instance Eq QName where
  Plain v       == Plain v'         =   v==v'
  Qualified q v == Plain v'         =   v==v'
  Plain v       == Qualified q' v'  =   v==v'
  Qualified q v == Qualified q' v'  =   v==v' && q==q'

showQN :: Bool -> QName -> String
showQN _ (Plain n)           = showString n ""
showQN False (Qualified q n) = showString n ""
showQN True  (Qualified q n) = (showString q . showChar '.' . showString n) ""

type Label = (String,FileNode)	-- The label on an SExp contains two components,
				-- a unique string, and the file pointer.
				-- The former enables unique highlighting, and
				-- the latter enables highlighting of sharing.

data SExp a
  = SApp a [SExp a]		-- n-ary application of at least 2 expressions
  | SId a QName SFixity		-- an identifier (variable or constructor)
  | SLiteral a String		-- any other kind of basic value
  | SString a String Ellipsis	-- character strings have special sugar
  | SWithin a [SExp a]		-- chains of if/case/guard inside an expression
  | SLambda a			-- a lambda expression
  | SDoLambda a			-- (internal) the lambda binding in a do stmt
  | SDoStmt a a (SExp a) (SExp a) -- do { val <- stmt }
  | SIf a (SExp a) (Maybe (SExp a))	-- possibly contains result
  | SCase a (SExp a) (Maybe (SExp a))
  | SGuard a (SExp a) (Maybe (SExp a))
  | SFieldExpr a (SExp a) [String] [SExp a]
				-- constructor value, or update
  | SCut a			-- cut off subexpression (to limit depth)
  | SUnevaluated a		-- underscore
  | SInterrupted a		--  ^C  (expr entered but never completed)
  | SBottom a			-- _|_  (expr entered but never completed)
  | SCycle a String (SExp a)	-- cyclic expression shown as `id where id = ..'
  | SEquation a (SExp a) (SExp a)
			-- an equation only makes sense as the root of an SExp
  | SParens a (SExp a) Int	-- parenthesised expr
  | SInfinite a
  | SFiniteMap a [([SExp a],SExp a)]
   deriving Show		-- only for testing/debugging

type Ellipsis = Bool		-- is a character string truncated?

data SFixity = 
  SInfix Int | SInfixL Int | SInfixR Int | SAssoc Int String | SInfixDefault
  -- need own type for some hardcoded operators that are known to be
  -- semantically associative
  deriving (Show)

-- translate fixity from the file representation to the structured fixity type
transFixity :: Int -> SFixity
transFixity f = case f `divMod` 4 of
                  (p,0) -> SInfix p
                  (p,1) -> SInfixR p
                  (p,2) -> SInfixL p
                  (p,3) -> SInfixDefault

-- arity of an S-expression
arity :: SExp a -> Int
arity (SApp _ exps)     = length exps
arity (SWithin _ exps)  = length exps
arity (SIf _ _ _)       = 1
arity (SCase _ _ _)     = 1
arity (SGuard _ _ _)    = 1
arity (SEquation _ _ _) = 2
arity (SFieldExpr _ _ _ upds) = 1 + length upds
arity (SDoStmt _ _ _ _) = 3
arity _                 = 0

-- get child of an S-expression
-- precondition: i < arity exp
child :: Int -> SExp a -> SExp a
child i (SApp _ exps)    = skipCaseIfGuard (exps!!i)
child i (SWithin _ exps) = skipCaseIfGuard (exps!!i)
--child 0 (SIf _ exp)      = exp
--child 0 (SCase _ exp)    = exp
--child 0 (SGuard _ exp)   = exp
child 0 (SEquation _ l r) = l
child 1 (SEquation _ l r) = r
child 0 (SFieldExpr _ e _ upds) = e
child i (SFieldExpr _ e _ upds) = upds!!(i-1)
child 0 (SDoStmt _ m _ _) = SLambda m
child 1 (SDoStmt _ _ v _) = v
child 2 (SDoStmt _ _ _ a) = a
child i exp               = errorT ("SExp.child: "++show i++" is too large.")

skipCaseIfGuard (SIf _ exp _) = exp
skipCaseIfGuard (SCase _ exp _) = exp
skipCaseIfGuard (SGuard _ exp _) = exp
skipCaseIfGuard exp = exp

label :: SExp a -> a
label (SApp l _)      = l
label (SId l _ _)     = l
label (SLiteral l _)  = l
label (SString l _ _) = l
label (SLambda l)     = l
label (SWithin l _)   = l
label (SIf l _ _)     = l
label (SCase l _ _)   = l
label (SGuard l _ _)  = l
label (SCut l)        = l
label (SUnevaluated l)= l
label (SInterrupted l)= l
label (SBottom l)     = l
label (SCycle l _ _)  = l
label (SDoStmt l _ _ _) = l
label (SEquation l _ _) = l
label (SFieldExpr l _ _ _) = l

children :: SExp a -> [SExp a]
children (SApp _ es)             = es
children (SWithin l es)          = es
children (SIf _ e _)             = [e]
children (SCase _ e _)           = [e]
children (SGuard _ e _)          = [e]
children (SCycle _ _ e)          = [e]
children (SEquation _ e r)       = [e,r]
children (SFieldExpr _ e _ upds) = e: upds
children (SDoStmt _ m e p)       = [SDoLambda m,e,p]
children _                       = []

rebuild :: SExp a -> [SExp a] -> SExp a
rebuild (SApp l _)       es = SApp l es
rebuild (SWithin l _)    es = SWithin l es
rebuild (SIf l _ r)     [e] = SIf l e r
rebuild (SCase l _ r)   [e] = SCase l e r
rebuild (SGuard l _ r)  [e] = SGuard l e r
rebuild (SCycle l v _)  [e] = SCycle l v e
rebuild (SEquation l _ _) [e,r] = SEquation l e r
rebuild (SFieldExpr l _ labs _) (e:upds) = SFieldExpr l e labs upds
rebuild (SDoStmt l _ _ _) [SDoLambda m,e,p] = SDoStmt l m e p
rebuild sexp             _  = sexp

relabel :: a -> SExp a -> SExp a
relabel l (SApp _ es)       = SApp l es
relabel l (SId _ v f)       = SId l v f
relabel l (SLiteral _ s)    = SLiteral l s
relabel l (SString _ s d)   = SString l s d
relabel l (SLambda _)       = SLambda l
relabel l (SDoLambda _)     = SDoLambda l
relabel l (SWithin _ e)     = SWithin l e
relabel l (SIf _ e r)       = SIf l e r
relabel l (SCase _ e r)     = SCase l e r
relabel l (SGuard _ e r)    = SGuard l e r
relabel l (SCut _)          = SCut l
relabel l (SUnevaluated _)  = SUnevaluated l
relabel l (SInterrupted _)  = SInterrupted l
relabel l (SBottom _)       = SBottom l
relabel l (SCycle _ a b)    = SCycle l a b
relabel l (SDoStmt _ m e p) = SDoStmt l m e p
relabel l (SEquation _ e r) = SEquation l e r
relabel l (SFieldExpr _ e labs upds) = SFieldExpr l e labs upds

parent :: SExp Label -> FileNode
parent (SWithin _ (x:_)) = parent x
parent (SEquation _ x _) = parent x
parent x                 = getParentNode (snd (label x))

prune :: Int -> SExp a -> SExp a
prune 0 s = SCut (label s)
prune n (SApp a es) = SApp a (head es: map (prune (n-1)) (tail es))
prune n (SWithin a es) = SWithin a (head es: map (prune (n-1)) (tail es))
prune n (SEquation a lhs rhs) = SEquation a (prune n lhs) (prune n rhs)
prune n s = s

funId :: SExp a -> QName
funId (SApp _ es) = funId (head es)
funId (SWithin _ es) = funId (head es)
funId (SEquation _ e _) = funId e
funId (SFieldExpr _ e _ _) = funId e
funId (SString _ _ _) = Qualified "Prelude" ":"
funId (SLambda _) = Plain "(\\..)"
funId (SDoStmt _ _ _ _) = Plain "do"
funId (SId _ s _) = s
funId _ = Plain ""

funLabel :: SExp Label -> FileNode
funLabel (SApp _ es) = funLabel (head es)
funLabel (SWithin _ es) = funLabel (head es)
funLabel (SEquation _ e _) = funLabel e
funLabel (SFieldExpr _ e _ _) = funLabel e
funLabel (SString (_,n) _ _) = n
funLabel (SDoStmt _ (_,n) _ _) = n
funLabel (SLambda (_,n)) = n
funLabel (SId (_,n) _ _) = n
funLabel _ = LowLevel.nil

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

-- conversion function
-- If `uneval' boolean is True, then unevaluated arguments appear in full
-- in the result, otherwise they are represented by SUnevaluated.
fileNode2SExp :: Int -> Bool -> Bool -> Bool -> Label -> SExp Label
fileNode2SExp cutoff uneval strings toplevelLHS labl =
  case go cutoff uneval strings toplevelLHS [] labl of (e,_,_) -> e  
  where
  simple e = (e,[],[])
  go :: Int	            -- cutoff depth
     -> Bool                -- show unevaluated args in full?
     -> Bool                -- sugar character strings?
     -> Bool                -- top-level LHS? (implies uneval to one level)
     -> [(FileNode,String)] -- enclosing nodes w/ variable name for `where'
     -> Label               -- root node of expression
     -> ( SExp Label        -- expression 
        , [FileNode]        -- nodes that start cycle
        , [String] )        -- variable names occurring (except for cycles)
  go 0     uneval strings top nodesAbove labl = simple (SCut labl)
  go depth uneval strings top nodesAbove labl@(lab,node) =
    if      node == LowLevel.nil then simple $ SUnevaluated labl
    else if node == unevaluated then simple $ SUnevaluated labl
    else if node == entered then simple $ SBottom labl
    else if node == interrupted then simple $ SInterrupted labl
    else if node == lambda then simple $ SLambda labl
    else if node == dolambda then simple $ SDoLambda labl
    else
    let r = peekResult node in
    if  r == unevaluated && not (uneval||top) then simple $ SUnevaluated labl
    else if r == entered && not top then simple $ SBottom labl
    else if r == interrupted && top then simple $ SInterrupted labl
    else if r == lambda && not uneval then simple $ SLambda labl
    else if r == dolambda && not uneval then simple $ SDoLambda labl
    else case simpleNodeType node of
      NodeModule -> "got a Module" `errorAt` node
      NodeSrcPos -> "got an SrcPos" `errorAt` node
      NodeAtom -> -- "got an Atom" `errorAt` node
        let i = getAtom node
            m = getAtomMod node
        in simple (SId labl (Qualified m i) (transFixity (getAtomFixity node)))
      NodeApplication -> 
        let partCycles :: [FileNode]
            partCycles = (funCycles ++ concat argsCycles)
            partVars :: [String]
            partVars = funVars ++ concat argsVars
            isCycle = node `elem` partCycles
            var :: String
            var = head . filter (not . (`elem` partVars)) .
                  map (("cyc"++) . show) $ [1..] 
            newNodesAbove :: [(FileNode,String)]
            newNodesAbove = (node,var) : nodesAbove
            subExps :: [FileNode]
            subExps = getSubExprs node
            -- (fun,funCycles,funVars) = ... not accepted by nhc98
            fun = fst3 z
            funCycles = snd3 z
            funVars = thd3 z
            z = let f = head subExps in
                if f==LowLevel.nil then simple (SCut ('f':lab,f))
                else go depth uneval strings False newNodesAbove ('f':lab,f)
            args = fst3 zs
            argsCycles = snd3 zs
            argsVars = thd3 zs
            -- (args,argsCycles,argsVars) = ... not accepted by nhc98
            zs = unzip3 $ map (go (depth-1) uneval strings False newNodesAbove)
                              (zipWith lbl ['1'..] (tail subExps))
            lbl c n = (c:lab, n)
            -- To do strings right, need to peek one level inside a cons.
            z1 = go 1 uneval strings False newNodesAbove
                 ('1':lab, subExps!!1)	-- only used in string cutoff case
            z2 = go 3 uneval strings False newNodesAbove
                 ('2':lab, subExps!!2)	-- only used in string cutoff case

            sexp = case fun of
              -- convert the representation of constructors with fields
              SId n@(_,m) c _ | isConstrFields m ->
                SFieldExpr labl (SId n c SInfixDefault)
                                 (getFieldLabels m) args
              -- convert char-list into string
              SId _ (Qualified _ ":") _ | strings && length args == 2 ->
                case args!!0 of
                  SLiteral _ c | not (null c) && head c == '\'' ->
                    case args!!1 of
                      SId _ (Qualified _ "[]") _
                                    -> SString labl (init (tail c)) False
                      SString _ s d -> SString labl (init (tail c)++s) d
                      _             -> SApp labl (fun:args)
                  SCut _ ->	-- peek beyond the cut
                    case fst3 z1 of
                      SLiteral _ c | not (null c) && head c == '\'' ->
                        case fst3 z2 of
                          SId _ (Qualified _ "[]") _
                             -> SString labl (init (tail c)) False
                          _  -> SString labl (init (tail c)) True
                      _ -> SApp labl (fun:args)
                  _ -> SApp labl (fun:args)
              -- different bracketing of a char-list
              SApp _ args1@[SId _ (Qualified _ ":") _,SLiteral _ c]
                | strings && length args == 1
                && not (null c) && head c == '\'' ->
                    case args!!0 of
                      SId _ (Qualified _ "[]") _
                                    -> SString labl (init (tail c)) False
                      SString _ s d -> SString labl (init (tail c)++s) d
                      _             -> SApp labl (args1++args)
              SApp _ args1@[SId _ (Qualified _ ":") _,SCut _]
                | strings && length args == 1 ->
                    fst3 (go (depth+1) uneval strings False nodesAbove labl)
              -- combine applications
              SApp n args1 -> SApp labl (args1++args)
              -- convert DoLambda to DoStmt
        --    SDoLambda n -> let binding = args!!0 in
              SId n (Qualified _ "do") _ ->
                    let binding = args!!0
                        bindingFN = snd (label binding)
                        nearestEnclosing node =
                            let par = getParentNode node
                                res = getResult par False
                            in if res==bindingFN
                               then nearestEnclosing par else res
                    in
                    SDoStmt labl n binding
                            (fst3 (go depth uneval strings top []
                                      ("d",(nearestEnclosing bindingFN))))
              -- anything else is just a simple application
              _ -> SApp labl (fun:args)
        
        -- Check for cycles in the application and return appropriately.
        -- The case for no cycles is Nothing then the else branch.
        in case lookup node nodesAbove of
             Just var -> (SId labl (Plain var) SInfixDefault,[node],[]) 
                         -- `lower' end of cycle
             Nothing  -> ( if isCycle then SCycle (lab,node) var sexp else sexp
                         , partCycles
                         , partVars)
      NodeBasicValue ->
          let i = getValue node in simple $ SLiteral labl i
      NodeIdentifier ->
          if isLambda node then simple (SLambda labl) else
          let i = getValue node
              m = getValueMod node
          in
          ( SId labl (Qualified m i) (case i of
                           "."  | m == "Prelude" -> SAssoc 9 i
                           "++" | m == "Prelude" -> SAssoc 5 i
                           "&&" | m == "Prelude" -> SAssoc 3 i
                           "||" | m == "Prelude" -> SAssoc 2 i
                           "*"  | m == "Prelude" -> SAssoc 7 i
                           "+"  | m == "Prelude" -> SAssoc 6 i
                           ">>" | m == "Prelude" -> SAssoc 1 i
                           ">>=" | m == "Prelude" -> SAssoc 1 i
                           _ -> transFixity (getFixity node))
          , []
          , if isConstructor node then [] else [i] )
      NodeCAF ->
          let i = getValue node
              m = getValueMod node
          in simple (SId labl (Qualified m i) (transFixity (getFixity node)))
      NodeConditional ->
          let skind = case nodeType node of
                          ExpGuard -> SGuard
                          ExpCase  -> SCase
                          ExpIf    -> SIf
              within :: Label -> (SExp Label->Maybe (SExp Label)->SExp Label)
                        -> SExp Label -> SExp Label -> SExp Label
              within labl kind parent exp =
                case parent of	-- eliminate chains of SWithin
                    SWithin _ ps -> SWithin labl (ps++[kind exp Nothing])
                    _            -> SWithin labl [parent,kind exp Nothing]
          in simple $
             within labl (skind ('c':lab,node))
                          (fileNode2SExp depth uneval strings True
                                         ('w':lab, getParentNode node))
                          (fileNode2SExp depth uneval strings False
                                         ('v':lab, head (getSubExprs node)))
      NodeSugar -> -- simple $ SLiteral labl "{sugar}"
          case nodeType node of
            ExpDoStmt -> simple $ SLiteral labl "{do stmt}"
            ExpFieldUpdate ->
                let (exp:updValues) = getSubExprs node
                    updLabels = getFieldLabels node
                in simple $
                   SFieldExpr labl
                              (fileNode2SExp (depth-1) uneval strings False
                                             ('u':lab, exp))
                              updLabels
                              (map (fileNode2SExp (depth-1) uneval
                                                  strings False)
                                   (zipWith (\i v-> (i:lab, v))
                                            ['1'..] updValues))
      NodeSpecial ->
          case nodeType node of
            ExpProjection ->
                (\(exp,x,y) -> (relabel ('p':lab,node) exp, x, y) ) $
                go depth uneval strings False nodesAbove
                   ('p':lab, getResult (head (getSubExprs node)) top)
            ExpHidden -> 
                simple $ SLiteral labl "{?}"
            ExpForward ->
                go depth uneval strings False nodesAbove
                   (lab, head (getSubExprs node))

  errorAt :: String -> FileNode -> a
  errorAt str node = errorT ("SExp: "++str++" at 0x"++showHex (int node) "")


-- useful document combinators:
-- non-breaking space
(<->) :: Doc -> Doc -> Doc
d1 <-> d2 = d1 <> delimiter " " <> d2 

-- breakable space
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> fdelimiter " " <> d2

-- breakable non-space
(<|>) :: Doc -> Doc -> Doc
d1 <|> d2 = d1 <> fdelimiter "" <> d2

-- breakable before a comma
(<*>) :: Doc -> Doc -> Doc
d1 <*> d2 = d1 <|> text "," <> d2

-- breakable before or after a cons
(<:>) :: Doc -> Doc -> Doc
d1 <:> d2 = d1 <|> text ":" <|> d2

indentation :: Int
indentation = 2


isOpSym :: String -> Bool
isOpSym ""  = True	-- representation of the unit value ()
isOpSym sym = let c = head sym in
              not (isAlpha c || c `elem` "[_{" || sym == "(\\..)") 

funDoc :: Bool -> QName -> Doc
funDoc q qn = (if isOpSym var then parens else id) $ text (showQN q qn)
  where var = case qn of Plain v -> v; Qualified _ v -> v

opDoc :: Bool -> QName -> Doc
opDoc q qn =  text (if isAlpha (head var) then ('`' :showQN q qn++ "`")
                    else showQN q qn)
  where var = case qn of Plain v -> v; Qualified _ v -> v


data ArgPos = ALeft | ARight
isRight ARight = True
isRight ALeft = False
isLeft = not . isRight

-- surround by parentheses if necessary
-- first fixity of surrounding expression, then if left or right argument,
-- then fixity of expression itself
optParens :: SFixity -> ArgPos -> SFixity -> Doc -> Doc
optParens surFixity aPos ownFixity =
  case (priority surFixity) `compare` (priority ownFixity) of
    LT -> if priority surFixity == (-1) then groupNest indentation else id
    GT -> groupNest indentation . parens
    EQ -> if (isInfixR surFixity && isInfixR ownFixity && isRight aPos)
            || (isInfixL surFixity && isInfixL ownFixity && isLeft aPos)
            || sameAssoc surFixity ownFixity 
            then id
            else groupNest indentation . parens

{-
minimalParens :: Parenthesiser
minimalParens (SApp va (id@(SId vt (Qualified _ (',':xs)) _):args))
  = if length xs + 2 != length args
      then SParens va (SApp va (minParens id):(map minParens args))
      else SApp va (minParens id):(map minParens args)
-}
schemeParens :: SExp a -> SExp a
schemeParens = par 0
  where
    par :: Int -> SExp a -> SExp a
    par d (SApp a xs)         = SParens a (SApp a (map (par (d+1)) xs)) d
    par d (SWithin a xs)      = SWithin a (map (par (d+1)) xs)
    par d (SDoStmt a b xa xb) = SParens a (SDoStmt a b (par (d+1) xa)
                                                       (par (d+1) xb)) d
    par d (SIf a c r)         = SParens a (SIf a (par (d+1) c) r) d
    par d (SCase a c r)       = SParens a (SCase a (par (d+1) c) r) d
    par d (SGuard a c r)      = SParens a (SGuard a (par (d+1) c) r) d
    par d (SFieldExpr a x ms bs) = SParens a (SFieldExpr a (par (d+1) x) ms
                                                         (map (par (d+1)) bs)) d
    par d (SCycle a s x)      = SCycle a s (par d x)
    par d (SEquation a x y)   = SParens a (SEquation a (par (d+1) x)
                                                       (par (d+1) y)) d
    par _ x = x

notCondParens :: SExp a -> SExp a
notCondParens = par 0
  where
    par d (SApp a xs)         = SApp a (map (allow d) xs)
    par d (SWithin a xs)      = SWithin a (map (allow d) xs)
    par d (SDoStmt a b xa xb) = SDoStmt a b (allow d xa) (allow d xb)
    par d (SIf a c r)         = SIf a (par d c) r
    par d (SCase a c r)       = SCase a (par d c) r
    par d (SGuard a c r)      = SGuard a (par d c) r
    par d (SFieldExpr a x ms bs) = SFieldExpr a (allow d x) ms
                                              (map (allow d) bs)
    par d (SCycle a s x)      = SCycle a s (allow d x)
    par d (SEquation a x y)   = SEquation a (allow d x) (allow d y)
    par _ x = x

    allow d (SApp a xs)         = SParens a (SApp a (map (allow (d+1)) xs)) d
    allow d (SWithin a xs)      = SWithin a (map (allow d) xs)
    allow d (SDoStmt a b xa xb) = SDoStmt a b (allow d xa) (allow d xb)
    allow d (SIf a c r)         = SParens a (SIf a (par (d+1) c) r) d
    allow d (SCase a c r)       = SParens a (SCase a (par (d+1) c) r) d
    allow d (SGuard a c r)      = SParens a (SGuard a (par (d+1) c) r) d
    allow d (SFieldExpr a x ms bs) = SFieldExpr a (allow d x) ms
                                                (map (allow d) bs)
    allow d (SCycle a s x)      = SCycle a s (allow d x)
    allow d (SEquation a x y)   = SParens a (SEquation a (allow (d+1) x)
                                                         (allow (d+1) y)) d
    allow _ x = x


sameAssoc :: SFixity -> SFixity -> Bool
sameAssoc (SAssoc _ var1) (SAssoc _ var2) = True
sameAssoc _ _ = False

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 f c [] = c
foldr0 f c xs = foldr1 f xs

listDoc :: (Show a, Eq a) => Bool -> (a->Doc->Doc) -> SFixity -> ArgPos -> SExp a -> Doc
listDoc qual high surFixity aPos e =
  group $ text "[" <> commas e
  where
  commas = sExpFold (sExp2Doc False True qual high) (text ",") (text "]")

  sExpFold head cons nil (SApp v [SId c (Qualified _ ":") _, hd
                                 , SId n (Qualified _ "[]") _]) =
      high v (head hd <|> high c (high n nil))
  sExpFold head cons nil (SApp v [SId c (Qualified _ ":") _, hd
                                 , SId n (Plain cyc) _])
      | "cyc" `isPrefixOf` cyc =
      high v (head hd <|> high c cons <|> high n (text (cyc++"..."))
      <|> high c (high n nil))
  sExpFold head cons nil (SApp v [SId c (Qualified _ ":") _
                                 , hd@(SCut t), SCut n]) =
      high v (head hd <> high c (high t (text "...")) <|> high n nil)
  sExpFold head cons nil (SApp v [SId c (Qualified _ ":") _, hd, tl]) =
      high v (head hd <|> high c cons <|> sExpFold head cons nil tl)
  sExpFold head cons nil (SId n (Qualified _ "[]") _) =
      high n nil
  sExpFold head cons nil e@(SUnevaluated _) =
      sExp2Doc False True qual high e <|> nil
  sExpFold head cons nil e@(SLiteral _ "{?}") =
      sExp2Doc False True qual high e<|>nil
  sExpFold head cons nil e =
      sExp2Doc False True qual high e

setDoc :: (Show a, Eq a) => Bool -> (a->Doc->Doc) -> SFixity -> ArgPos -> SExp a -> Doc
setDoc qual high surFixity aPos e =
  group $ text "{" <> (addCommas $ elems e) <> text "}"
  where
  addCommas [] = nil
  addCommas [x] = sExp2Doc False True qual high x
  addCommas (x:xs) =    (sExp2Doc False True qual high x)
                     <> text ","
                     <> addCommas xs
  
  elems (SId v (Qualified "Data.FiniteMap" "EmptyFM") _) = []
  elems (SApp v [SId c (Qualified "Data.FiniteMap" "Branch") _, k, _, _, l, r])
    = k : elems l ++ elems r

fmDoc :: (Show a, Eq a) => Bool
                       -> (a->Doc->Doc)
                       -> SFixity
                       -> ArgPos
                       -> SExp a
                       -> Doc
fmDoc qual high surFixity aPos e =
  group $ text "{" <> (addCommas $ elems e) <> text "}"
  where
  addCommas [] = nil
  addCommas [(k,e)] =
        sExp2Doc False True qual high k
    <+> text "|->"
    <+> sExp2Doc False True qual high e
  addCommas ((k,e):xs) =
        sExp2Doc False True qual high k
    <+> text "|->"
    <+> sExp2Doc False True qual high e
    <>  text ","
    <>  addCommas xs
  
  elems (SId v (Qualified "Data.FiniteMap" "EmptyFM") _) = []
  elems (SApp v [SId c (Qualified "Data.FiniteMap" "Branch") _, k, e, _, l, r])
    = (k,e) : elems l ++ elems r

priority (SInfix p) = p
priority (SInfixL p) = p
priority (SInfixR p) = p
priority (SAssoc p _) = p
priority SInfixDefault = 9

isInfixL (SInfixL _) = True
isInfixL SInfixDefault = True
isInfixL _ = False

isInfixR (SInfixR _) = True
isInfixR _ = False

isNotInfixDefault SInfixDefault = False
isNotInfixDefault _ = False

considerAsOperator :: QName -> SFixity -> Bool
considerAsOperator qname fixity = isOpSym var || isNotInfixDefault fixity 
  where var = case qname of Plain v -> v; Qualified _ v -> v

highlightForDepth :: Int -> [Highlight]
highlightForDepth x = [head (drop ((x `mod` (length highlights))) highlights)]
  where
    highlights :: [Highlight]
    highlights = [ Foreground Red, Foreground Green, Foreground Cyan
                 , Foreground Magenta, Foreground Yellow ]

-- A central function.  Convert an SExpression to a Document using
-- the pretty-printing combinators.  In sugar mode, lists are shown
-- with [,,,] sugar, rather than in full with cons applications.
-- Colour mode = use coloured parentheses to highlight expr depth
sExp2Doc :: (Show a, Eq a) => Bool -> Bool -> Bool -> (a->Doc->Doc) -> SExp a -> Doc
sExp2Doc colour sugar qual high = goDoc (SInfix (-1)) ARight 
  where
  -- fixity of surrounding expression and which sort of argument
--goDoc :: SFixity -> ArgPos -> SExp a -> Doc
  goDoc surFixity aPos (SParens va item d) =
    if colour then
      (highlight (highlightForDepth d) (text "(")) <>
        (sExp2Doc colour sugar qual high item) <>
        (highlight (highlightForDepth d) (text ")"))
    else
      text "(" <>
      (sExp2Doc colour sugar qual high item) <>
      text ")"
  goDoc surFixity aPos (SInfinite va) =
    text "..."
  
  -- Tuples
  goDoc surFixity aPos (SApp va ((SId vt (Qualified _ (',':xs)) _):args)) =
    if length xs + 2 == length args 
    then high va $
         group (text "("
                 <> foldr1 comma (map (sExp2Doc colour sugar qual high) args)
               ) <> text ")"
           -- print tuple properly
    else high va $
         optParens surFixity aPos ownFixity
         . (high vt (text ("(,"++xs++")")) <+>)
         . foldr1 (<+>) . map (goDoc ownFixity ARight) $ args
           -- partial application of tuple constructor
    where
    ownFixity = SInfix 10
    comma l r = l <|> high vt (text ",") <> r
  -- Lists
  goDoc surFixity
             aPos
             (e@(SApp va [SId vf (Qualified _ ":") ownFixity,e1,e2]))
    | sugar && not (ambiguous e2)
      = high va $ listDoc qual high surFixity aPos e
    | otherwise
      = high va $ optParens surFixity aPos ownFixity
                            (goDoc ownFixity ALeft e1
                            <> high vf (text ":")
                            <> goDoc ownFixity ARight e2)
        where -- check whether the final tail of the list is _ or {?} or {^C}.
          ambiguous :: SExp a -> Bool
          ambiguous (SApp _ [SId _ (Qualified _ ":") _, e1, e2]) = ambiguous e2
          ambiguous (SCycle _ _ e) = ambiguous e
          ambiguous (SId _ (Qualified _ "{IO}") _) = True
          ambiguous (SId _ _ _) = False
          ambiguous (SCut _)            = False
          ambiguous (SLiteral _ "{?}")  = True
          ambiguous (SUnevaluated _)    = True
          ambiguous (SInterrupted _)    = True
          ambiguous (SInfinite _)       = True
          ambiguous (SBottom _)         = True
          ambiguous _                   = True	-- shouldn't happen!
  -- Sets
  goDoc surFixity aPos ((SApp va [SId vf (Qualified "Data.Set" "MkSet") ownFixity,s]))
    = high va $ setDoc qual high surFixity aPos s
  -- Finite Maps
  goDoc surFixity aPos e@(SId va (Qualified "Data.FiniteMap" "EmptyFM") ownFixity)
    = high va $ text "{}"
  goDoc surFixity aPos e@((SApp va [SId vf (Qualified "Data.FiniteMap" "Branch") ownFixity,k,el,s,l,r]))
    = high va $ fmDoc qual high surFixity aPos e
  
  goDoc surFixity aPos (SApp va [SId vf var ownFixity,e1,e2]) 
    | considerAsOperator var ownFixity = 
      high va $
      optParens surFixity aPos ownFixity
        (goDoc ownFixity ALeft e1
        <+> high vf (opDoc qual var)
        <+> goDoc ownFixity ARight e2)
  goDoc surFixity aPos (SApp va [SId vf var ownFixity,e]) 
    | considerAsOperator var ownFixity =
      -- show infix operator with single argument as section
      groupNest indentation . high va . parens $
        goDoc ownFixity ALeft e <-> high vf (opDoc qual var)
  goDoc surFixity aPos (SApp va (fun:args)) =
    high va
    . optParens surFixity aPos ownFixity
    . (goDoc ownFixity ALeft fun <+>)
    .  foldr1 (<+>) . map (goDoc ownFixity ARight) $ args
    where
    ownFixity = SInfix 10
  goDoc surFixity aPos (SDoStmt va m var par) =
    high va $
    bold (high m (text "do")) <+> goDoc surFixity ARight var
    <+> bold (text "<-") <+> goDoc surFixity ARight par
  goDoc _ _ (SLambda v) = high v $ text "(\\..)"
  goDoc _ _ (SId v var fixity) = high v $ funDoc qual var
  goDoc _ _ (SString v s d) = high v $ text "\"" <> text s <>
                              (if d
                               then highlight [Foreground Blue] (text "...")
                               else nil) <>
                              text "\""
  goDoc _ _ (SLiteral v lit) = high v $ text lit
  goDoc _ _ (SWithin v es) = 
    high v $ foldr1 (\a b-> a <+> group (bold (text "|") <+> b))
                    (map (sExp2Doc colour sugar qual high) es)
  goDoc _ _ (SIf v exp@(SId vl (Qualified "Prelude" "False") fix) (Just res)) =
    (groupNest indentation . high v) (bold (text "if")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "else")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SIf v exp@(SId vl (Qualified "Prelude" "True") fix) (Just res)) =
    (groupNest indentation . high v) (bold (text "if")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "then")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SIf v exp Nothing) = 
    (groupNest indentation . high v) (bold (text "if")
                                     <+> sExp2Doc colour sugar qual high exp)
  goDoc _ _ (SCase v exp@(SId vl (Qualified "Prelude" "False") fix)
                     (Just res@(SCase _ _ _))) =
    (groupNest indentation . high v) (bold (text "case")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "->")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SCase v exp@(SId vl (Qualified "Prelude" "False") fix)
                     (Just res)) =
    (groupNest indentation . high v) (bold (text "case")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "otherwise")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SCase v exp@(SId vl (Qualified "Prelude" "True") fix)
                     (Just res)) =
    (groupNest indentation . high v) (bold (text "case")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "then")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SCase v exp Nothing) = 
    (groupNest indentation . high v) (bold (text "case")
                                     <+> sExp2Doc colour sugar qual high exp)
  goDoc _ _ (SGuard v exp@(SId vl (Qualified "Prelude" "False") fix)
                      (Just res)) =
    (groupNest indentation . high v) (bold (text "|")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "->")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SGuard v exp@(SId vl (Qualified "Prelude" "True") fix)
                      (Just res)) =
    (groupNest indentation . high v) (bold (text "|")
                                     <+> sExp2Doc colour sugar qual high exp
                                     <+> bold (text "=")
                                     <+> sExp2Doc colour sugar qual high res)
  goDoc _ _ (SGuard v exp Nothing) = 
    (groupNest indentation . high v) (bold (text "|")
                                     <+> sExp2Doc colour sugar qual high exp)
  goDoc _ _ (SCut v) = high v $ highlight [ReverseVideo] (text " ")
  goDoc _ _ (SUnevaluated v) = high v $ text "_"
  goDoc _ _ (SBottom v) = high v $ text "_|_"
  goDoc _ _ (SInterrupted v) =
      highlight [Foreground Blue] $ high v $ text "{^C}"
  goDoc _ _ (SCycle v var exp) = 
    groupNest indentation . high v . parens $
      text var <+> group (bold (text "where" <+> text var <+> text "=")
                          <+> sExp2Doc colour sugar qual high exp)
  goDoc _ _ (SEquation v lhs rhs) =
    group (high v (sExp2Doc colour sugar qual high lhs
                  <+> text "=" <+> sExp2Doc colour sugar qual high rhs))
  goDoc _ _ (SFieldExpr v e labs upds) =
    high v $ group (sExp2Doc colour sugar qual high e
                    <> text "{" <> commas (zipWith field labs upds)
                    <> text "}")
    where
      field name value = text name <> text "="
                         <> sExp2Doc colour sugar qual high value
      commas [] = Pretty.nil
      commas [doc] = doc
      commas (doc:docs) = doc <> text "," <+> commas docs
  goDoc _ _ (SFiniteMap v maps) =
    group (text "{" <> commas (reZipWith mapping maps) <> text "}")
    where 
      mapping args res = foldr1 (<+>)
                                (map (sExp2Doc colour sugar qual high) args)
                         <> text "->"
                         <> sExp2Doc colour sugar qual high res
      commas [] = Pretty.nil
      commas [doc] = doc
      commas (doc:docs) = doc <> text "," <+> commas docs
      reZipWith f [] = []
      reZipWith f ((x,y):rs) = f x y:reZipWith f rs
  goDoc _ _ x = text (show x)


{- Pretty-print an expression with no interior highlighting -}
prettyExpression :: String -> Int -> Options -> FileNode -> String
prettyExpression initial width
                 Options { cutoffDepth=cutoff, unevalMode=uneval
                         , stringSugar=strSugar, listSugar=listSugar
                         , showQual=qual }
                 node =
  pretty width
         (highlight [Foreground Blue] (text initial)
         <> groupNest (length initial)
                      (sExp2Doc False listSugar qual nohigh
                                (fileNode2SExp cutoff uneval
                                               strSugar False ("",node))))
  where nohigh _ doc = doc

prettySExp :: (Show a, Eq a) => String -> Int -> Options -> SExp a -> String
prettySExp initial width
           Options { cutoffDepth=cutoff, unevalMode=uneval
                   , stringSugar=strSugar, listSugar=listSugar
                   , showQual=qual, colourBracks=colouringOn}
           exp =
  pretty width
         (highlight [Foreground Blue] (text initial)
         <> groupNest (length initial)
                      (sExp2Doc colouringOn listSugar qual nohigh exp))
  where nohigh _ doc = doc

{- Pretty-print an equation with no interior highlighting -}
prettyEquation :: String -> String -> Int -> Options -> FileNode -> String
prettyEquation initial final width
               Options { cutoffDepth=cutoff, unevalMode=uneval
                       , stringSugar=strSugar, listSugar=listSugar
                       , showQual=qual }
               node =
  pretty width
         ( highlight [Foreground Blue] (text initial)
         <> groupNest (length initial)
                      ( sExp2Doc False listSugar qual nohigh
                                 (fileNode2SExp cutoff uneval strSugar True
                                                ("",node))
                        <-> text "=" <->
                        sExp2Doc False listSugar qual nohigh
                                 (fileNode2SExp cutoff uneval strSugar False
                                                ("",getResult node True)))
         <> (if null final then nil
             else delimiter "  " <> highlight [Foreground Blue] (text final)))
  where nohigh _ doc = doc

{- Pretty-print an equation with no interior highlighting -}
-- a bit of a quick hack for hat-source
-- all in one line and allows highlighting of whole equation
prettyEquation2 :: Options -> FileNode -> String
prettyEquation2 
               Options { cutoffDepth=cutoff, unevalMode=uneval
                       , stringSugar=strSugar, listSugar=listSugar
                       , showQual=qual }
               node = 
  pretty 10000
           (group
                      ( sExp2Doc False listSugar qual nohigh
                                 (fileNode2SExp cutoff uneval strSugar True
                                                ("",node))
                        <-> text "=" <->
                        sExp2Doc False listSugar qual nohigh
                                 (fileNode2SExp cutoff uneval strSugar False
                                                ("",getResult node True))))
  where nohigh _ doc = doc


{-
-- only for testing:
test1 =
  SApp (mkHatNode 0)
    [ SId (mkHatNode 1) "fun" SInfixDefault
    , SLiteral (mkHatNode 2) "24"
    , SLiteral (mkHatNode 3) "True"
    , SApp (mkHatNode 4)
        [ SId (mkHatNode 5) "+" (SInfixL 5)
        , SLiteral (mkHatNode 6) "3"
        , SLiteral (mkHatNode 7) "4"
        ]
    ]

test2 =
  SApp (mkHatNode 10)
    [ SId (mkHatNode 11) "*" (SInfixL 6)
    , SApp (mkHatNode 12)
        [ SId (mkHatNode 13) "+" (SInfixL 5)
        , SApp (mkHatNode 14)
            [ SId (mkHatNode 15) "-" (SInfixL 5)
            , SLiteral (mkHatNode 16) "3"
            , SLiteral (mkHatNode 17) "6"
            ]
        , SApp (mkHatNode 18)
            [ SId (mkHatNode 19) "-" (SInfixL 5)
            , SLiteral (mkHatNode 20) "3"
            , SLiteral (mkHatNode 21) "6"
            ]
        ]
    , test1
    ]
-}
