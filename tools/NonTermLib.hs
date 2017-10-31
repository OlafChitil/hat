module NonTermLib
  ( 
  -- FileNode Functions
    FileNode(..)
  , getNodeChildren
  , getRootNode
  , peekResultMod
  , getResultHT
  , nextFileNode
  , showNodeType

  -- function node functions
  , getFuncPtr
  , getFuncName
  , getFuncModule

  -- State
  , State(..)
  , emptyState
  , setOptions

  -- SExp Functions
  , SExp(..)
  , getSExpNode 
  , fileNode2SExpMod

  -- Sctx
  , Sctx
  , showSctx

  -- Monad functions
  , initialiseCount
  , incrementCount
  , getCount
  , printAndFalse
  , printList
  , mapIO

  -- auxilliary functions
  , inter
  , member
  , remove
  , breakAfter

  ) where
 

import SExp        hiding (transFixity)
import SrcRef             (SrcRef(..),readSrcRef)
import LowLevel
import Foreign.C.String   (CString(..), peekCString)
import System.IO.Unsafe   (unsafePerformIO)
import Data.List          (isPrefixOf)
import Data.Char          (ord)
import System.IO          (hPutStrLn,stderr)
import Numeric            (showHex)
import System.Exit        (exitWith,ExitCode(..))
import PrettyLibHighlight (Doc,groupNest,highlight,pretty,nest,text,(<>))
import qualified PrettyLibHighlight as Pretty (highlight)
import HighlightStyle     (goto,cls,clearDown,clearUp,cleareol,highlightOff
                          ,Highlight(..),Colour(..)
                          ,enableScrollRegion,savePosition,restorePosition)


-- FileNode functions

-- make filenodes printable, and orderable
-- instance Show FileNode where
--  show fn = showHex (int fn) ""

-- instance Ord FileNode where
--   (FileNode x) <= (FileNode y) = x <= y
--   (FileNode x) >= (FileNode y) = x >= y
--   (FileNode x) < (FileNode y)  = x < y
--   (FileNode x) > (FileNode y)  = x > y


getNodeChildren :: FileNode -> [FileNode]
getNodeChildren n = remove n (map (getImmediateExpArg n) [0..arity])
  where arity = getExpArity n

foreign import ccall "nontermutils.h"
        getImmediateExpArg :: FileNode -> Int -> FileNode
foreign import ccall "nontermutils.h"
        getExpArity   :: FileNode -> Int
foreign import ccall "nontermutils.h"
        getRootNode :: FileNode
foreign import ccall "nontermutils.h"
        peekResultMod :: FileNode -> FileNode
foreign import ccall "nontermutils.h"
        getResultHT :: FileNode -> Bool -> FileNode
foreign import ccall "nontermutils.h"
        nextFileNode :: FileNode -> FileNode


-- Show the type of a node
showNodeType node | node <= (FileNode 4)        = "special"
                | ntype == ExpApp               = "ExpApp"
                | ntype == ExpValueApp          = "ExpValueApp"
                | ntype == ExpValueUse          = "ExpValueUse"
                | ntype == ExpConstUse          = "ExpConstUse"
                | ntype == ExpConstDef          = "ExpConstDef"
                | ntype == ExpGuard             = "ExpGuard"
                | ntype == ExpCase              = "ExpCase"
                | ntype == ExpIf                = "ExpIf"
                | ntype == ExpForward           = "ExpForward"
                | ntype == ExpProjection        = "ExpProjection"
                | ntype == Module               = "Module"
                | ntype == SrcPos               = "SrcPos"
                | ntype == ExpChar              = "ExpChar"
                | ntype == ExpInt               = "ExpInt"
                | ntype == ExpInteger           = "ExpInteger"
                | ntype == ExpRat               = "ExpRat"
                | ntype == ExpRational          = "ExpRational"
                | ntype == ExpFloat             = "ExpFloat"
                | ntype == ExpDouble            = "ExpDouble"
                | ntype == ExpFieldUpdate       = "ExpFieldUpdate"
                | ntype == ExpHidden            = "ExpHidden"
                | ntype == ExpDoStmt            = "ExpDoStmt"
                | ntype == AtomVariable         = "AtomVariable"
                | ntype == AtomConstructor      = "AtomConstructor"
                | ntype == AtomAbstract         = "AtomAbstract"
  where ntype = nodeType node


-- get a pointer to a the function definition for an expression
foreign import ccall "nontermutils.h" getFuncPtr :: FileNode -> FileNode

-- get the name of a function from a NodeApplication
getFuncName :: FileNode -> String
foreign import ccall "nontermutils.h" getFuncNm :: FileNode -> CString 
getFuncName n = unsafePerformIO (peekCString (getFuncNm n))

-- get the module of a function from a NodeApplication
getFuncModule :: FileNode -> String
foreign import ccall "nontermutils.h" getFuncMod :: FileNode -> CString 
getFuncModule n = unsafePerformIO (peekCString (getFuncMod n))


-- SExp Stuff

{- requires overlapping instances:
-- test SExps for equality
instance Eq (SExp Label) where
  x == y = (getSExpNode x) == (getSExpNode y)
-}

-- get the filenode from an SExp
getSExpNode sexp = snd (label sexp)


-- The state datatype holds all of the configuration data for the tools
data State = State
        { progname  :: String           -- program name
        , file      :: FilePath         -- .hat filename
        , width     :: Int              -- terminal screen size
        , height    :: Int              -- terminal screen size
        , strSugar  :: Bool             -- show strings with sugar?
        , listSugar :: Bool             -- show lists with sugar?
        , srcrefs   :: Bool             -- always show src references?
        , showQual  :: Bool             -- show identifiers qualified?
        , cutoff    :: Int              -- expression cutoff depth
        , numFn     :: Int              -- how many instances of fn
        , showRoot  :: Bool             -- cut off the head of the trail?
        , showNode  :: Bool             -- show the trail nodes?
        , showSrcRef:: Bool             -- show source ref?
        , showCount :: Bool             -- show number of nodes visited
        }

-- the emptyState function gives a state with all the defaults set
emptyState :: State
emptyState = State { strSugar=True, listSugar=True, srcrefs=True
                    ,showQual=False, cutoff=10, numFn=3, showRoot=False
                    ,showNode=False,showSrcRef=False,showCount=False
                    ,progname="", file="", width=80, height=25}

-- set state options from a list of command-line arguments
setOptions :: [String] -> State
setOptions (_:args) = setOpt args
  where
    setOpt [] = emptyState
    setOpt (x:xs) | "--showqual" `isPrefixOf` x = state {showQual = bool}
                  | "--cutoff" `isPrefixOf` x   = state {cutoff = num}
                  | "--numfn" `isPrefixOf` x    = state {numFn = num}
                  | "--showrt" `isPrefixOf` x   = state {showRoot = bool}
                  | "--shownode" `isPrefixOf` x = state {showNode = bool}
                  | "--srcref" `isPrefixOf` x   = state {showSrcRef = bool}
                  | "--showcount" `isPrefixOf` x = state {showCount = bool}
                  | otherwise                   = setOpt xs
      where bool = if (val /= [] && ((head val) == 'f' || (head val) == 'F')) 
                    then False else True
            num = decimalStringToInt val
            val = breakAfter ('='==) x
            state = setOpt xs


-- The stack context There is actually already an Sctx used by (iirc)
-- hat-trail. This is because I originally used that type. I should
-- change the name really.  Anyway, this type is used so that all of the
-- filenodes corresponding to an expression are bunched together in the
-- search process
type Sctx = [FileNode]

-- convert a stack context to a string with highlighting This is really
-- the central display function. Most of these functions are reused
-- from Hat.
showSctx :: State -> Sctx -> String
showSctx state ctx@(node:_) | (showSrcRef state) = str ++ srcref
                            | otherwise          = str
  where
    -- these four expressions turn the Sctx into a string with highlighting
    root = fileNode2SExpMod (cutoff state) True 
                            (strSugar state) True ("",node) 
    doc = sExp2Doc False (listSugar state) (showQual state) nodehigh root
    prompt = Pretty.highlight [Foreground Blue] (text "> ")
    str = pretty (width state) (prompt <> doc)

    nodehigh = high (last ctx)

    -- create a source reference string
    srcref | line == 0 && col == 0 = " " ++ (showNodeType (snd (label root))) ++ " (no ref)"
           | otherwise             = " (L" ++ (show col) 
                                      ++ ",C" ++ (show line) ++ ")"
      where sr   = readSrcRef (expSrcRef root)
            line = SrcRef.line sr
            col  = SrcRef.column sr

    -- highlight an SExp node, depending on its filenode value
    high :: FileNode -> Label -> Doc -> Doc
    high node v 
      | (snd v) == node = Pretty.highlight [Bold,Foreground Red]
      | otherwise       = id

-- get a source reference node from another filenode
expSrcRef :: SExp Label -> FileNode
expSrcRef (SEquation _ lhs rhs) = expSrcRef lhs
expSrcRef exp = getSrcRef node
  where node = (snd (label exp))


-- Monad functions

-- initialise the counter which counts the number of node accesses 
foreign import ccall "nontermutils.h" initialiseCount :: IO ()
foreign import ccall "nontermutils.h" getCount :: Int

-- increment the node access count by the argument value
foreign import ccall "nontermutils.h" incCount :: Int -> IO ()
incrementCount :: Int -> Bool
incrementCount x = unsafePerformIO (do incCount x
                                       return True)

-- DEBUGGING
-- print the value, and return false. Can be used at the start of conditional 
-- expressions to print out important values.
-- And yes, I know that's basically what Hood does.
printAndFalse :: Show x => x -> Bool
printAndFalse x = unsafePerformIO printFunc
  where printFunc = do print x
                       return False

-- print all the items of a list
printList :: Show x => [x] -> IO ()
printList xs = mapIO print xs

-- map a monadic function over a list
-- I'm pretty sure something like this must already exist
mapIO :: (a -> IO ()) -> [a] -> IO ()
mapIO funcIO [] = return ()
mapIO funcIO (x:xs) = do funcIO x
                         mapIO funcIO xs


-- Auxilliary functions

-- interpolate two lists
inter xs [] = []
inter [] ys = []
inter (x:xs) (y:ys) = (x:y:(inter xs ys))

-- test membership
member x [] = False
member x (y:ys) = x == y || member x ys

-- remove something from a list
remove x [] = []
remove x (y:ys) | x == y    = ys
                | otherwise = (y : (remove x ys))


-- This function is a modified version of the function used in Hat.  The
-- modifications are basically limited to commenting out two lines,
-- so that fileNode2SExpMod doesn't replace expressions which have an
-- interrupted / bottom result with a placeholder
fileNode2SExpMod :: Int -> Bool -> Bool -> Bool -> Label -> SExp Label
fileNode2SExpMod cutoff uneval strings toplevelLHS label =
  case go cutoff uneval strings toplevelLHS [] label of (e,_,_) -> e  
  where
  simple e = (e,[],[])
  go :: Int                     -- cutoff depth
       -> Bool                  -- show unevaluated args in full?
       -> Bool                  -- sugar character strings?
       -> Bool                  -- top-level LHS? (implies uneval to one level)
       -> [(FileNode,String)]   -- enclosing nodes w/ variable name for `where'
       -> Label                 -- root node of expression
       -> ( SExp Label          -- expression 
          , [FileNode]          -- nodes that start cycle
          , [String] )          -- variable names occurring (except for cycles)
  go 0     uneval strings top nodesAbove label = simple (SCut label)
  go depth uneval strings top nodesAbove label@(lab,node) =
    if      node == LowLevel.nil then simple $ SUnevaluated label
    else if node == unevaluated then simple $ SUnevaluated label
    else if node == entered then simple $ SBottom label
    else if node == interrupted then simple $ SInterrupted label
    else if node == lambda then simple $ SLambda label
    else if node == dolambda then simple $ SDoLambda label
    else
    let r = peekResult node in
    if  r == unevaluated && not (uneval||top) then simple $ SUnevaluated label
    -- these lines were commented out for black-hat / hat-nonterm
    --else if r == entered && not top then simple $ SBottom label
    --else if r == interrupted && top then simple $ SInterrupted label
    else if r == lambda && not uneval then simple $ SLambda label
    else if r == dolambda && not uneval then simple $ SDoLambda label
    else 
    case simpleNodeType node of
      NodeModule -> "got a Module" `errorAt` node
      NodeSrcPos -> "got an SrcPos" `errorAt` node
      NodeAtom -> -- "got an Atom" `errorAt` node
        let i = getAtom node
            m = getAtomMod node
        in simple (SId label (Qualified m i) (transFixity (getAtomFixity node)))
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
            subExps = getNodeChildren node
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
                 ('1':lab, subExps!!1)  -- only used in string cutoff case
            z2 = go 3 uneval strings False newNodesAbove
                 ('2':lab, subExps!!2)  -- only used in string cutoff case

            sexp = case fun of
              -- convert the representation of constructors with fields
              SId n@(_,m) c _ | isConstrFields m ->
                SFieldExpr label (SId n c SInfixDefault)
                                 (getFieldLabels m) args
              -- convert char-list into string
              SId _ (Qualified _ ":") _ | strings && length args == 2 ->
                case args!!0 of
                  SLiteral _ c | not (null c) && head c == '\'' ->
                    case args!!1 of
                      SId _ (Qualified _ "[]") _
                                    -> SString label (init (tail c)) False
                      SString _ s d -> SString label (init (tail c)++s) d
                      _             -> SApp label (fun:args)
                  SCut _ ->     -- peek beyond the cut
                    case fst3 z1 of
                      SLiteral _ c | not (null c) && head c == '\'' ->
                        case fst3 z2 of
                          SId _ (Qualified _ "[]") _
                             -> SString label (init (tail c)) False
                          _  -> SString label (init (tail c)) True
                      _ -> SApp label (fun:args)
                  _ -> SApp label (fun:args)
              -- different bracketing of a char-list
              SApp _ args1@[SId _ (Qualified _ ":") _,SLiteral _ c]
                | strings && length args == 1
                && not (null c) && head c == '\'' ->
                    case args!!0 of
                      SId _ (Qualified _ "[]") _
                                    -> SString label (init (tail c)) False
                      SString _ s d -> SString label (init (tail c)++s) d
                      _             -> SApp label (args1++args)
              SApp _ args1@[SId _ (Qualified _ ":") _,SCut _]
                | strings && length args == 1 ->
                    fst3 (go (depth+1) uneval strings False nodesAbove label)
              -- combine applications
              SApp n args1 -> SApp label (args1++args)
              -- anything else is just a simple application
              _ -> SApp label (fun:args)
        in case lookup node nodesAbove of
             Just var -> (SId label (Plain var) SInfixDefault,[node],[]) 
                         -- `lower' end of cycle
             Nothing -> ( if isCycle then SCycle (lab,node) var sexp else sexp
                        , partCycles, partVars)
      NodeBasicValue ->
          let i = getValue node in simple $ SLiteral label i
      NodeIdentifier ->
          if isLambda node then simple (SLambda label) else
          let i = getValue node
              m = getValueMod node
          in
          ( SId label (Qualified m i) (case i of
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
          in simple (SId label (Qualified m i) (transFixity (getFixity node)))
      NodeConditional ->
          let skind = case nodeType node of
                          ExpGuard -> SGuard
                          ExpCase  -> SCase
                          ExpIf    -> SIf
              within :: Label -> (SExp Label->Maybe (SExp Label)->SExp Label)
                        -> SExp Label -> SExp Label -> SExp Label
              within labl kind parent exp =
                case parent of
                    SWithin _ ps -> SWithin labl (ps++[kind exp Nothing])
                    _            -> SWithin labl [parent,kind exp Nothing]
          in simple $
             within label (skind ('c':lab,node))
                          (fileNode2SExpMod depth uneval strings True
                                         ('w':lab, getParentNode node))
                          (fileNode2SExpMod depth uneval strings False
                                         ('v':lab, head (getSubExprs node)))
      NodeSugar -> -- simple $ SLiteral label "{sugar}"
          case nodeType node of
            ExpDoStmt -> simple $ SLiteral label "{do stmt}"
            ExpFieldUpdate ->
                let (exp:updValues) = getSubExprs node
                    updLabels = getFieldLabels node
                in simple $
                   SFieldExpr label
                              (fileNode2SExpMod (depth-1) uneval strings False
                                             ('u':lab, exp))
                              updLabels
                              (map (fileNode2SExpMod (depth-1) uneval
                                                  strings False)
                                   (zipWith (\i v-> (i:lab, v))
                                            ['1'..] updValues))
      NodeSpecial ->
          case nodeType node of
            ExpProjection ->
                (\(exp,x,y) -> (relabel ('p':lab,node) exp, x, y) ) $
                go depth uneval strings False nodesAbove
                   ('p':lab, getResult (head (getSubExprs node)) top)
            ExpHidden -> simple $ SLiteral label "{?}"
            ExpForward ->
                go depth uneval strings False nodesAbove
                   (lab, head (getSubExprs node))

  errorAt :: String -> FileNode -> a
  errorAt str node = errorT ("SExp: "++str++" at 0x"++showHex (int node) "")


-- translate fixity from the file representation to the structured
-- Sfixity type. This is another steal from Hat. It is needed to make 
-- fileNode2SExpMod work.
transFixity :: Int -> SFixity
transFixity f = case f `divMod` 4 of
                  (p,0) -> SInfix p
                  (p,1) -> SInfixR p
                  (p,2) -> SInfixL p
                  (p,3) -> SInfixDefault

-- see above re. fileNode2SExpMod
fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

-- see above re. fileNode2SExpMod
relabel :: a -> SExp a -> SExp a
relabel l (SApp _ es)     = SApp l es
relabel l (SId _ v f)     = SId l v f
relabel l (SLiteral _ s)  = SLiteral l s
relabel l (SString _ s d) = SString l s d
relabel l (SLambda _)     = SLambda l
relabel l (SWithin _ e)   = SWithin l e
relabel l (SIf _ e r)     = SIf l e r
relabel l (SCase _ e r)   = SCase l e r
relabel l (SGuard _ e r)  = SGuard l e r
relabel l (SCut _)        = SCut l
relabel l (SUnevaluated _)= SUnevaluated l
relabel l (SInterrupted _)= SInterrupted l
relabel l (SBottom _)     = SBottom l
relabel l (SCycle _ a b)  = SCycle l a b
relabel l (SEquation _ e r) = SEquation l e r
relabel l (SFieldExpr _ e labs upds) = SFieldExpr l e labs upds

-- see above re. fileNode2SExpMod
errorT  :: String -> a
errorT s = unsafePerformIO (do hPutStrLn stderr s; return (error ""))
debugT  :: (Show a) => String -> a -> a
debugT s x = unsafePerformIO (do hPutStrLn stderr (s++show x); return x)


-- Turn a string of decimal characters into an integer value 
decimalStringToInt digits = decimalStringToInt' digits 0
decimalStringToInt' [] acc = acc
decimalStringToInt' (d:digits) acc 
  | d >= '0' && d <= '9' 
                = decimalStringToInt' digits ((10 * acc) + (ord d - ord '0'))
  | otherwise   = -1


-- cut everything up to and including whatever matches condition f
breakAfter f [] = []
breakAfter f (x:xs) | (f x)     = xs
                    | otherwise = breakAfter f xs

