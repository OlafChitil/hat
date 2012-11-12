module Main where	-- HatTrail main program

import LowLevel		(openHatFile,getBridgeValue,hatVersionNumber
			,FileNode(..),nil,peekTrace,getResult,getParentNode
			,getErrorLoc,getErrorMessage
			,getSrcRef,getDefnRef)
import SrcRef		(SrcRef(..),readSrcRef,defnSrcRef)
import Ident		(Ident,getIdentAt)
import SExp		(SExp(..),Label,fileNode2SExp,sExp2Doc,arity,child
			,label,rebuild,children,parent,prune,funId,funLabel
			,QName(..),showQN)
import PrettyLibHighlight (Doc,pretty,nest,text,(<>))
import qualified PrettyLibHighlight as Pretty (highlight)
import HighlightStyle	(goto,cls,clearDown,clearUp,cleareol,highlightOff
			,highlight,Highlight(..),Colour(..)
			,enableScrollRegion,getTerminalSize
			,savePosition,restorePosition)
import Control.Exception(catch,IOException)
import Control.Monad	(when,liftM)
import System.Cmd       (system)
import System.Environment (getArgs,getProgName,getEnv)
import System.Exit      (exitWith,ExitCode(..))
import System.Directory	(doesFileExist)
import Data.List	(isPrefixOf,isSuffixOf,group,groupBy)
import System.IO	(hSetBuffering,BufferMode(..),stdin,stdout,stderr
			,hPutStrLn,hFlush)
import Numeric	        (showHex)
import Foreign.C.String (withCString)
import CommonUI		(hatTrail,hatObserve,hatDetect,hatAnim,hatView
                        ,hatExplore
			,Options(..),initialOptions
			,OptionCmd(..),optionCmd,onOff,number
			,optionsUpdate,showOption,showOnOff)


-- The recurring state within the main loop of the program contains
-- the .hat filename, the terminal width and height, preferred
-- highlighting styles, etc.
data State = State
	{ file      :: FilePath		-- .hat filename
	, width     :: Int		-- terminal screen size
	, height    :: Int		-- terminal screen size
	, startLine :: Int		-- first line of the trail pane
        , styleNew  :: [Highlight]	-- highlight for current subexpr
        , styleOld  :: [Highlight]	-- highlight for previous subexprs
	, srcrefs   :: Bool		-- always show src references?
	, options   :: Options		-- common configurable display options
	}

setState :: Mode -> State -> State
setState (O o)       state = state {options=optionsUpdate o (options state)}
setState (SrcRefs b) state = state {srcrefs=b}

showState :: Mode -> State -> String
showState (O o)       state = showOption o (options state)
showState (SrcRefs b) state = "  "++highlight [Underscore] "srcrefs"
                              ++ showOnOff b
                              ++ "srcrefs shown in status line"

-- The main hat-trail program
main = do
    args    <- System.Environment.getArgs
    prog    <- System.Environment.getProgName
    hatfile <- case args of (f:_) -> return (rectify f)
                            _     -> do hPutStrLn stderr
                                                  ("hat-trail: no trace file")
                                        exitWith (ExitFailure 1)
    withCString prog (\p-> withCString hatfile (openHatFile p))
    errloc <- getErrorLoc
    errmsg <- getErrorMessage
    output <- readOutputFile hatfile
    bridge <- readBridgeFile
    (style0,style1) <- catch (do home <- getEnv "HOME"
                                 f <- readFile (home++"/.hattrailrc")
                                 return (read f))
                             (\e-> (e :: IOException) `seq`
                                   return ( [Dim,Foreground Red]
                                          , [Bold,Foreground Magenta] ))
    (columns,lines) <- getTerminalSize
    let state = State { file=hatfile, width=columns, height=lines, startLine=0
	              , srcrefs=True, styleOld=style0, styleNew=style1
                      , options=initialOptions {equations=False} }
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    System.Cmd.system ("stty -icanon min 1 -echo")
    case args of
      [f,"-remote",n] -> remote state (read n)
      _ -> begin state errloc errmsg output (map peekTrace bridge)
  where
    begin :: State -> FileNode -> String -> String -> [FileNode] -> IO ()
    begin state errloc errmsg output bridge =
        do (state',root) <- chooseFromOutput state errloc errmsg output bridge
           let res = getResult root False
           if equations (options state) then
               if res == nil then
                    loop state' [( startLine state'
                                 , Sctx (SEquation ("=",nil)
                                                   (toSExp state True "l" root)
                                                   (SBottom ("b",res))) [])]
               else loop state' [( startLine state'
                                 , Sctx (SEquation ("=",nil)
                                                   (toSExp state True "l" root)
                                                   (toSExp state False "r" res))
                                        []
                                 )]
             else
             loop state' [ (startLine state', Sctx (toSExp state True "l" root)
                                                   []) ]
        where
           loop state stack = do repaint state stack
                                 (state',stack') <- selectSubExpr state stack
                                 case stack' of
                                  [] -> begin state' errloc errmsg output bridge
                                  _  -> loop state' stack'

    remote :: State -> Int -> IO ()
    remote state root =
        do putStr (highlight [Bold]
                             ("Trail: "++ replicate (width state - 8) '-'))
           let node = FileNode root
               res = getResult node False
           if equations (options state) then
               if res==nil then
                    loop state{startLine=2}
                         [( 2, Sctx (SEquation ("=",nil)
                                               (toSExp state True "l" node)
                                               (SBottom ("b",res)))
                                    [] )]
               else loop state{startLine=2}
                         [( 2, Sctx (SEquation ("=",nil)
                                               (toSExp state True "l" node)
                                               (toSExp state False "r" res))
                                    [] )]
             else loop state{startLine=2}
                       [(2, Sctx (toSExp state True "l" node) [])]
        where
           loop state stack = do repaint state stack
                                 (state', stack') <- selectSubExpr state stack
                                 case stack' of
                                   [] -> loop state' stack
                                   _  -> loop state' stack'
  
    rectify :: FilePath -> FilePath
    rectify f | ".hat" `isSuffixOf` f = f
              | otherwise = f ++ ".hat"

    readOutputFile :: FilePath -> IO String
    readOutputFile hat = do readFile (hat++".output")

    readBridgeFile :: IO [FileNode]
    readBridgeFile = do until (==nil) getBridgeValue
      where
        until :: (a->Bool) -> IO a -> IO [a]
        until pred action = do
            v <- action
            if pred v then return [] else do vs <- until pred action
                                             return (v:vs)

-- Restore the terminal before quitting
resetSystem :: State -> IO ()
resetSystem state = do
    putStr (enableScrollRegion 1 (height state))
    putStr (goto 1 (height state))
    System.Cmd.system ("stty icanon echo")
    return ()


-- Conversion of a filenode to an S-expression
toSExp :: State -> Bool -> String -> FileNode -> SExp Label
toSExp state@State{options=opts} lhs l n =
    fileNode2SExp (cutoffDepth opts) (unevalMode opts)
                  (stringSugar opts) lhs (l,n)


-- Allow the user to select a redex trail starting from the program output.
-- This implementation allows movement one clump of characters at a time,
-- where the clumps are based on having the same trace pointer.
chooseFromOutput :: State -> FileNode -> String -> String -> [FileNode]
                    -> IO (State,FileNode)
chooseFromOutput state errloc errmsg output bridge =
  do
    putStr (cls ++ goto 1 1)
    when (isError)
         (do putStrLn (highlight [Bold]
                                 ("Error: "++replicate (width state-8) '-'))
             putStrLn errmsg)
    when (not (null output))
         (do putStrLn (highlight [Bold]
                                 ("Output: "++replicate (width state-9) '-')))
          -- putStrLn output)
    putStr (goto 1 (startLine-1))
    putStr (highlight [Bold] ("Trail: "++replicate (width state-8) '-'))
    statusLine ("hat-trail "++hatVersionNumber++"   (:h for help, :q to quit)")
               state{startLine=startLine}
    putStr (enableScrollRegion startLine (height state))
    (state',i) <- select state{startLine=startLine} 0
    return (state', clumpNodes!!i)
  where
    select :: State -> Int -> IO (State,Int)
    select st i = do
        putStr (location i (styleNew st) clumpStrs ++ goto 1 startLine)
        c <- getCommand st
        case c of
          Movement L    -> if i==0 then select st 0 else select st (i-1)
          Movement Up   -> if i==0 then select st 0 else select st (i-1)
          Movement R    -> if i==max then select st max else select st (i+1)
          Movement Down -> if i==max then select st max else select st (i+1)
          Select -> do putStr (location i (styleOld st) clumpStrs)
                       return (st,i)
          Status -> do showCurrentSettings st
                       select st i
          Set m  -> do st' <- newSetting m st
                       select st' i
          Help   -> do showHelp st
                       putStr (goto 1 startLine ++ clearDown)
                       select st i
          Quit   -> do resetSystem st
                       exitWith ExitSuccess
          _      -> select st i

    isError   = errloc /= nil
    lenError  = if not isError then 0 else linesize (width state) errmsg + 1
    lenOutput = if null output then 0 else linesize (width state) output + 1
    startLine = lenError + chunkSize + 3
    chunkSize = let chunk = height state `div` 4 in
                if lenOutput > chunk then chunk else lenOutput
    max = length clumpStrs - 1
    clumpNodes = (if isError then (errloc:) else id) $
                 map head (group bridge)
--  clumpStrs  = (if isError then ((errmsg ++ goto 1 (1+lenError)):) else id) $
--               map (showNL . map fst)
--                   (groupBy (\(_,n) (_,m)-> n==m) (zip output bridge))
    clumpStrs  = (if isError then (errmsg:) else id) $
                 beginOutput $
                 map (showNL . map fst)
                     (groupBy (\(_,n) (_,m)-> n==m) (zip output bridge))
    showNL [] = []
    showNL ('\n':cs) = '\\':'n':'\n': showNL cs
    showNL (c:cs)    = c: showNL cs
    location i style [] = ""
    location i style xs =
        let (as,bs) = splitAt i xs
            size | isError && i<=chunkSize = lenError+chunkSize
                 | otherwise               = chunkSize
        in
        concatMap (\n-> goto 1 n ++ cleareol)	-- clean output window
                  [2+lenError .. startLine-2]
        ++ goto 1 2
        -- previously had   safeUnlines (width state) ...
        ++ unlines (take size (trim size (lines (concat as))
                               ++ lines ((highlight style (head bs))
                                          ++ concat (tail bs))))
        ++ highlightOff
    trim :: Int -> [String] -> [String]
    trim n pref = if length pref >= n then beginOutput (trim n (drop n pref))
                                      else pref
    safeUnlines w [] = []
    safeUnlines w (x:xs) = (if length x > w then take (w-4) x ++ "...\n"
                            else x++"\n") ++ safeUnlines w xs
    beginOutput  []    = []
    beginOutput (s:ss) = (goto 1 (2+lenError) ++ s): ss
 -- beginOutput  []    = [ concatMap (\n-> goto 1 n ++ cleareol)
 --                                  [2+lenError .. startLine-2]
 --                        ++ goto 1 (2+lenError) ]
 -- beginOutput (s:ss) = (concatMap (\n-> goto 1 n ++ cleareol)
 --                                 [2+lenError .. startLine-2]
 --                        ++ goto 1 (2+lenError)
 --                        ++ s)
 --                      : ss
    linesize width str = foldr (addsize width) 0 (map length (lines str))
    addsize width n total = total + (n `div` (width+1)) + 1




-- An S-expression context is the means of navigation through an expression.
-- It contains the "current" node, and a stack of parent nodes, each with
-- an annotation to say which child of that parent is on the direct path from
-- the root to the current node.
data Sctx = Sctx (SExp Label) [(Int, SExp Label)]

getRoot :: Sctx -> SExp Label
getRoot (Sctx s []) = s
getRoot (Sctx _ xs) = let (_,r) = last xs in r


-- Allow the user to select a subexpression from the given expression.
-- Takes a stack of previous full expressions, and returns a new stack,
-- either pushing the new subexpression, or popping an old one.
selectSubExpr :: State -> [(Int,Sctx)] -> IO (State,[(Int,Sctx)])
selectSubExpr state stack@((lineno,sctx@(Sctx e _)):_) =
    loop (label e) sctx
  where
    loop :: Label -> Sctx -> IO (State,[(Int,Sctx)])
    loop lab@(labl,node) ctx@(Sctx e rest) = do
        let (len,str,exp) = paintOne state highlightWithSharing ctx
            extent = lineno + len
            realLineNo = if extent <= height state
                         then lineno
                         else height state - len
            newctx | null rest = Sctx exp []
                   | otherwise = ctx
        when (srcrefs state) (showSrcRef e state)
        putStr (goto 1 realLineNo ++ clearDown ++ str ++ goto 1 realLineNo)
        interpret lab newctx extent
    interpret :: Label -> Sctx -> Int -> IO (State,[(Int,Sctx)])
    interpret lab@(_,node) ctx@(Sctx e ctx') extent = do
        c <- getCommand state
        case c of
          Quit -> do resetSystem state
                     exitWith ExitSuccess
          Movement m ->
                    do let newctx@(Sctx exp _) = moveSelection m ctx
                       when (srcrefs state) (showSrcRef exp state)
                       loop (label exp) newctx
          Select -> let par = parent e in
                    if par == nil then
                         do statusLine "no parent" state
                            interpret lab ctx extent
                    else let res = getResult par False in
                      if equations (options state) then
                        if res==nil then
                          return (state, (( extent
                                          , Sctx (SEquation ("=",nil)
                                                     (toSExp state True "l" par)
                                                     (SBottom ("b",res)))
                                                 []
                                          )
                                         : (lineno, ctx): tail stack))
                        else
                          return (state, (( extent
                                          , Sctx (SEquation ("=",nil)
                                                   (toSExp state True "l" par)
                                                   (toSExp state False "r" res))
                                                 []
                                           )
                                         : (lineno, ctx): tail stack))
                      else return (state, (( extent
                                           , Sctx (toSExp state True "l" par) []
                                           )
                                          : (lineno, ctx): tail stack))
          Delete -> let stack' =
                          case ctx of
                            Sctx (SEquation _ lhs rhs) ctx'
                                | not (equations (options state)) ->
                                 (lineno,Sctx lhs ctx'):tail stack
                            Sctx _ [(_, SEquation _ lhs rhs)] ->
                                 (lineno,Sctx lhs []):tail stack
                            _ -> tail stack
                    in do
                    -- if extent >= height state
                    --   then repaint state stack'
                    --   else putStr (goto 1 lineno ++ clearDown)
                       return (state,stack')
          Shrink -> let newctx = Sctx (cut lab e)
                                      (map (\(i,e)->(i,cut lab e)) ctx')
                    in do repaint state ((lineno,newctx):tail stack)
                          loop (label e) newctx
          Expand -> let newctx = Sctx (join state lab e)
                                      (map (\(i,e)->(i,join state lab e)) ctx')
                    in do repaint state ((lineno,newctx):tail stack)
                          loop (label e) newctx
          Revert -> let newctx = Sctx (revert state lab e)
                                      (map (\(i,e)->(i,revert state lab e)) ctx')
                    in do repaint state ((lineno,newctx):tail stack)
                          loop (label e) newctx
          Detect ->  do System.Cmd.system (hatDetect (file state) node)
                        interpret lab ctx extent
          Trail  ->  do System.Cmd.system (hatTrail (file state) node)
                        interpret lab ctx extent
          Anim   ->  do System.Cmd.system (hatAnim (file state) node)
                        interpret lab ctx extent
          Explore ->  do System.Cmd.system (hatExplore (file state) node)
                         interpret lab ctx extent
          ObserveAll -> do System.Cmd.system (hatObserve (file state)
                                                     (showQN True (funId e)))
                           interpret lab ctx extent
          ObservePat p ->
                        do System.Cmd.system (hatObserve (file state) p)
                           interpret lab ctx extent
          ObserveSrc -> do let srcref = expSrcRef e
                           when (srcref /= nil)
                                (let sr = readSrcRef srcref in
                                 do System.Cmd.system
                                        (hatObserve (file state)
                                            (SrcRef.filename sr
                                            ++" "++show (SrcRef.line sr)
                                            ++" "++show (SrcRef.column sr)))
                                    return ())
                           interpret lab ctx extent
          Repaint -> do repaint state stack
                        loop lab ctx
          Resize -> resize state stack
          Source -> do let srcref = expSrcRef e
                       when (srcref /= nil)
                            (do System.Cmd.system (hatView (readSrcRef srcref))
                                showSrcRef e state)
                       interpret lab ctx extent
          Definition -> let defn = getDefnRef (funLabel e)
                        in if defn==nil then do
                            statusLine (showQN (showQual (options state))
                                               (funId e)
                                       ++": defn not available") state
                            interpret lab ctx extent
                        else do
                            defnSR <- liftM defnSrcRef (getIdentAt defn)
                            ok <- doesFileExist (SrcRef.filename defnSR)
                            if not ok then
                                statusLine (showQN (showQual (options state))
                                                   (funId e)
                                           ++": defn not found") state
                              else do
                                System.Cmd.system (hatView defnSR)
                                statusLine (showQN (showQual (options state))
                                                   (funId e)
                                           ++": definition") state
                            interpret lab ctx extent
          Result -> let r = getRoot ctx in
                    case r of
                      SEquation _ lhs rhs ->
                           do putStr (goto 1 lineno ++ clearDown)
                              return ( state
                                     , (lineno, (Sctx lhs [])) : tail stack )
                      _ -> let res = getResult (snd (label r)) False in
                           if res==nil then
                                let bot = SBottom ("b",FileNode 1) in
                                return ( state
                                       , ( lineno
                                         , (Sctx r [(0, SEquation ("=",nil)
                                                                  r bot)])
                                         ) : tail stack)
                           else let rhs = toSExp state False "r" res in
                                return ( state
                                       , ( lineno
                                         , (Sctx rhs [(1, SEquation ("=",nil)
                                                                     r rhs)])
                                         ) : tail stack)
          Status -> do showCurrentSettings state
                       repaint state stack
                       interpret lab ctx extent
          Set mode -> do state' <- newSetting mode state
                         let rootlabel = case getRoot ctx of
                                           SEquation _ lhs rhs -> label lhs
                                           expr                -> label expr
                         return ( state'
                                , ( ( lineno
                                    , Sctx (toSExp state' True "l"
                                                   (snd rootlabel))
                                           []
                                    ) : tail stack))
          Help -> do showHelp state
                     repaint state stack
                     loop lab ctx
          _ -> interpret lab ctx extent


-- To shrink or expand the current node in the S-expression, we must
-- shrink or expand it at every level in the context path as well.
cut :: Eq a => a -> SExp a -> SExp a
cut v exp = let l = label exp in
            if l==v then SCut v
                    else rebuild exp (map (cut v) (children exp))
join :: State -> Label -> SExp Label -> SExp Label
join state v exp = let l = label exp in
                   if l==v then toSExp state False (fst v) (snd v)
                           else rebuild exp (map (join state v) (children exp))

-- To revert a subexpression means to display it in its less-than-finally-
-- evaluated form.  In particular, rather than seeing a lambda expression
-- in the function position, you might prefer the higher-order composition that
-- led to the lambda.  We can /only/ do reversion if the parent of the
-- highlighted expression has the expression itself as its result, so there
-- is a direct equivalence of value.
revert :: State -> Label -> SExp Label -> SExp Label
revert state v exp =
    let l = label exp
        node = snd v
        par = getParentNode node
        res = getResult par False
    in if l==v  && res==node
       then toSExp state True (fst v) par
       else rebuild exp (map (revert state v) (children exp))

-- Movement within the S-expression tree can be specified in a couple of
-- different ways:
--   Pre-order traversal:
--     R:  if a branch, go down and left
--         if a leaf, go up and right
--     L:  if a branch, go up, left, and down to the right
--         if a leaf, go up, left, and down to the right
--   Level navigation:
--     Up:    go to the enclosing expr (if there is one)
--     Down:  go to the first interior subexpression (if there is one)
--     JumpR: go one expr to the right within the enclosing expr
--     JumpL: go one expr to the left within the enclosing expr
moveSelection :: Cursor -> Sctx -> Sctx
moveSelection R (Sctx s ctx) =
    if arity s > 0 then Sctx (child 0 s) ((0,s): ctx) else unwindr ctx
    where unwindr [] = Sctx s ctx
          unwindr ((i,p):ctx) =
              let i' = i+1 in
              if i' < arity p then Sctx (child i' p) ((i',p):ctx)
              else unwindr ctx
moveSelection L (Sctx s ctx) = unwindl ctx
    where unwindl [] = Sctx s []
          unwindl ((i,p):ctx) =
              let i' = i-1 in
              if i > 0 then windr (child i' p) ((i',p):ctx)
              else Sctx p ctx
          windr p ctx = let rhs = (arity p) - 1 in
                        if rhs >= 0 then windr (child rhs p) ((rhs,p):ctx)
                                    else Sctx p ctx
moveSelection JumpR ctx@(Sctx _ []) = ctx
moveSelection JumpL ctx@(Sctx _ []) = ctx
moveSelection JumpR ctx@(Sctx s ((i,p):ctx')) =
    let i' = i+1 in
    if i' < arity p then Sctx (child i' p) ((i',p):ctx') else ctx
moveSelection JumpL ctx@(Sctx s ((i,p):ctx')) =
    let i' = i-1 in
    if i > 0 then Sctx (child i' p) ((i',p):ctx') else ctx
moveSelection Up ctx@(Sctx _ [])           = ctx
moveSelection Up     (Sctx _ ((i,p):ctx')) = Sctx p ctx'
moveSelection Down ctx@(Sctx s ctx') =
    if arity s > 0 then Sctx (child 0 s) ((0,s):ctx') else ctx
moveSelection ListLeft (Sctx s ((0,SApp _ (SId _ (Qualified _ ":") _ : _))
                               :(1, p@(SApp _ (SId _ (Qualified _ ":") _
                                              : hd: tl)))
                               :ctx')) =
    Sctx hd ((0,p):ctx')
moveSelection ListRight (Sctx s ((0,p'@(SApp _ (SId _ (Qualified _ ":") _ : _:
                                       p@(SApp _ (SId _ (Qualified _ ":") _
                                                 : hd: tl)):_)))
                                :ctx')) =
    Sctx hd ((0,p):(1,p'):ctx')
moveSelection ListLeft  ctx | otherwise = ctx
moveSelection ListRight ctx | otherwise = ctx


-- Paint a single stack element (Int,Sctx) to a String.  While we're
-- at it, most callers want to know how many lines are used by the string.
paintOne :: State
            -> (State -> Label -> Label -> Doc -> Doc)	-- subexpr highlighter
            -> Sctx					-- abstract expression
            -> (Int,String,SExp Label)	-- (length, text, squashed expr)
paintOne state high ctx@(Sctx node _) =
    -- Find the root of the S-expr, then make it fit on the screen
    (squash state (high state (label node)) (cutoffDepth (options state))
    . getRoot) ctx
  where
    -- When an expression does not fit on screen, it must be progressively
    -- reduced in size (via cutoff depth) until it does fit.
    squash :: State -> (Label->Doc->Doc) -> Int -> SExp Label
              -> (Int,String,SExp Label)
    squash state high cut s =
        let       -- cut-down the expression to size
            exp = prune cut s
    	      -- paint SExp as a Doc, highlighting current subtree node
            doc = sExp2Doc False (listSugar (options state))
                           (showQual (options state)) high exp
    	      -- and then pretty-print it, with introductory marker
            str = pretty (width state - 2) (text "<- " <> nest 3 doc)
            len = length (lines str)
        in        -- now does it fit?
        if len <= height state - startLine state
        then (len,str,exp)
        else squash state high (cut`div`2) s

-- Various different styles of highlighting are required in different
-- screen contexts.
highlightOldSelection, highlightNewSelection, highlightWithSharing
    :: State -> Label -> Label -> Doc -> Doc
highlightOldSelection state node v =
    if v == node then Pretty.highlight (styleOld state) else id
highlightNewSelection state node v =
    if v == node then Pretty.highlight (styleNew state) else id
highlightWithSharing state (labl,nod) (l,n) =
    if l==labl then Pretty.highlight (styleNew state)
    else if n==nod then Pretty.highlight (styleOld state)
    else id


-- Repaint screen, or possibly start afresh and follow the same path
-- through the trails (e.g. save state and restore?)

-- This implementation of `repaint' tries to calculate only what
-- is visible on screen, rather than repainting the entire trail.
-- It starts painting at the bottom of the screen and goes upwards
-- until the screen is full, then stops.
repaint :: State -> [(Int,Sctx)] -> IO ()
repaint state [] =
    do putStr (goto 1 (startLine state) ++ clearDown)
repaint state ((expBegin,exp):stack) =
    do putStr (goto 1 start ++ clearDown)
       putStr (goto 1 (expBegin-(offset`max`0)) ++ str)
       if offset <= 0			-- everything fits on screen?
         then do putStr (goto 1 expBegin ++ str)
                 mapM_ paintAll stack
         else do putStr (goto 1 (expBegin-offset) ++ str)
                 mapM_ paintAll (visible stack)
                 putStrLn (goto 1 start ++ partial stack)
  where
    start = startLine state
    extent = expBegin + len
    offset = extent - height state	-- reduce absolute linenums to fit
    (len,str,_) = paintOne state highlightNewSelection exp -- most recent item

    paintAll i@(lineno,exp) =
        let (_,str,_) = paintOne state highlightOldSelection exp in
        putStr (goto 1 lineno ++ str)

    visible [] = []
    visible ((lineno,exp):rest) =
        let adjusted = lineno - offset in
        if adjusted >= start then (adjusted,exp): visible rest else []
    partial [] = ""
    partial ((lineno,exp):rest) =
        let adjusted = lineno - offset in
        if adjusted > start then partial rest	-- keep looking
        else if adjusted == start then ""	-- no partial trail
        else ( unlines				-- partial trail
             . drop (start - adjusted)
             . lines
             . (\(_,x,_)->x)
             . paintOne state highlightOldSelection ) exp
      

-- Repaint the screen following a resizing of the terminal.  This means
-- we need to recalculate all the line numbers, and we must repaint the
-- entire trail.
resize :: State -> [(Int,Sctx)] -> IO (State,[(Int,Sctx)])
resize state stack =
    do (columns,lines) <- getTerminalSize
       let state' = state {width=columns,height=lines}
       putStr (enableScrollRegion (startLine state') (height state'))
       putStr (goto 1 (startLine state') ++ clearDown)
       stack' <- paint state' (startLine state') [] (reverse stack)
       return (state',stack')
  where
    paint state _ acc [] = return acc
    paint state lineno acc ((_,ctx@(Sctx node _)):stack) =
        let (len,str,_) = paintOne state highlightOldSelection ctx
            extent = lineno + len
        in do
        if (extent < height state)
          then putStrLn (goto 1 lineno ++ str)	-- either careful placement
          else putStrLn str		-- or allow bottom of screen to scroll
        paint state extent ((lineno,ctx):acc) stack


-- Various status-line things, such as source reference printing, showing
-- the current settings, etc.

expSrcRef :: SExp Label -> FileNode
expSrcRef (SEquation _ lhs rhs) = expSrcRef lhs
expSrcRef exp = getSrcRef (snd (label exp))

showSrcRef :: SExp Label -> State -> IO ()
showSrcRef exp state =
    let srcref = expSrcRef exp in
    if srcref == nil
    then statusLine "no src reference" state
    else let sr = readSrcRef srcref in
         statusLine (SrcRef.filename sr ++" line: "++show (SrcRef.line sr)
                     ++" col: "++show (SrcRef.column sr)) state

statusLine :: String -> State -> IO ()
statusLine "" state =
    do putStr (savePosition ++ goto 15 (startLine state - 1)
               ++ highlight [Bold] (replicate 63 '-') ++ restorePosition)
statusLine msg state =
    do let dmsg = detab msg
       putStr (savePosition ++ goto 15 (startLine state - 1)
               ++ ' ': take 61 dmsg ++ " "
               ++ highlight [Bold] (replicate (max 0 (61 - length dmsg)) '-')
               ++ restorePosition)
       hFlush stdout
  where
    detab [] = []
    detab ('\t':cs) = " - "++detab cs
    detab (c:cs) = c: detab cs

showCurrentSettings :: State -> IO ()
showCurrentSettings state@State{options=opts} =
    do statusLine "current settings" state
       moreViewer (map (\m-> showState m state) modes) state
  where
    modes = [ O (Uneval True), O (Strings True), O (Lists True)
            , O (Qualify True), O (Equations True)
            , SrcRefs True, O (CutOff 0) ]

newSetting :: Mode -> State -> IO State
newSetting mode state =
    do let state' = setState mode state
       statusLine (showState mode state') state'
       System.Cmd.system ("sleep 1")
       return state'


-- A simple key-stroke interpreter.
data Cmd = Movement Cursor
         | Select | Delete  | Shrink | Expand | Revert
         | Reduce | Result  | Source | Definition
         | Detect | Trail   | Anim | Explore 
         | ObserveAll | ObservePat String | ObserveSrc
         | Quit   | Unknown | Help
         | Repaint  | Resize
         | Set Mode | Status

data Cursor = L | R | JumpL | JumpR | Up | Down | ListLeft | ListRight
data Mode   = SrcRefs Bool | O OptionCmd

getCommand :: State -> IO Cmd
getCommand state = do
    c <- getChar
    case c of
      'x'    -> return Quit
      '\n'   -> return Select
      ' '    -> return Reduce
      '\DEL' -> return Delete
      '\BS'  -> return Delete
      '-'    -> return Shrink
      '+'    -> return Expand
      '='    -> return Result
      '<'    -> return (Movement JumpL)
      '>'    -> return (Movement JumpR)
      ','    -> return (Movement JumpL)	-- unshifted <
      '.'    -> return (Movement JumpR)	-- unshifted >
      '['    -> return (Movement JumpL)
      ']'    -> return (Movement JumpR)
   -- '['    -> return (Movement ListLeft)
   -- ']'    -> return (Movement ListRight)
      '\^L'  -> return Repaint
      '\^R'  -> return Resize
      'r'    -> return Revert
      '\ESC' -> do
          c <- getChar
          case c of
            '[' -> do
                c <- getChar
                case c of
                  'D' -> return (Movement L)
                  'C' -> return (Movement R)
                  'A' -> return (Movement Up)
                  'B' -> return (Movement Down)
                  _   -> return Unknown
            'O' -> do
                c <- getChar
                case c of
                  'D' -> return (Movement L)
                  'C' -> return (Movement R)
                  'A' -> return (Movement Up)
                  'B' -> return (Movement Down)
                  _   -> return Unknown
            _   -> return Unknown
      ':' -> do
          putStr (savePosition ++ goto 1 (height state)++":")
          cmd <- colonCmd 1 ""
          putStr (goto 1 (height state) ++ cleareol ++ restorePosition)
          return cmd
      _   -> return Unknown

-- a specialised input mode on the bottom line of the screen for ':' commands
colonCmd :: Int -> String -> IO Cmd
colonCmd 0 _ = return Unknown
colonCmd n s = do
    let (w:ws) = words (reverse s)
    c <- getChar
    case c of
      '\n' | null s -> return Unknown
           | w `isPrefixOf` "source" -> return Source
           | w `isPrefixOf` "Source" -> return Definition
           | w `isPrefixOf` "set" ->
               case ws of
                 [] -> return Status
                 (s:ss) ->
                   case optionCmd ws of
                     Just o  -> return (Set (O o))
                     Nothing -> if s `isPrefixOf` "srcrefs"
                                then return (maybe Unknown Set
                                                   (onOff SrcRefs ss))
                                else return Unknown
           | w `isPrefixOf` "observe"  -> if null ws then return ObserveAll
                                          else return (ObservePat (unwords ws))
           | w `isPrefixOf` "location" -> return ObserveSrc
           | w `isPrefixOf` "trail"    -> return Trail
           | w `isPrefixOf` "animate"  -> return Anim
           | w `isPrefixOf` "detect"   -> return Detect
           | w `isPrefixOf` "explore"  -> return Explore
           | w `isPrefixOf` "resize"   -> return Resize
           | w `isPrefixOf` "quit" -> return Quit
           | w `isPrefixOf` "help" -> return Help
           | head w == '?' -> return Help
           | head w == '+' -> return (number (Set . O .Deeper) (tail w:ws) 1)
           | head w == '-' -> return (number (Set . O .Shallower) (tail w:ws) 1)
           | otherwise     -> return Unknown
           where (w:ws) = words (reverse s)
      '\DEL' -> do putStr "\DEL \DEL"
                   colonCmd (n-1) (tail s)
      '\BS'  -> do putStr "\BS \BS"
                   colonCmd (n-1) (tail s)
      _ -> do putChar c
              colonCmd (n+1) (c:s)


-- Help text is painted to screen here, but erased by the caller.
showHelp :: State -> IO ()
showHelp state =
    do statusLine "keystrokes/commands available" state
       moreViewer (lines helpText) state
  where helpText = "\
\ cursor keys   movement within current expression\n\
\ [ and ] keys  movement within current expression\n\
\ RETURN        show parent expression of selected expression\n\
\ =             show final result of the whole expression line\n\
\ BACKSPACE     remove most recently-added expression/equation\n\
\ -/+           shrink/expand a cutoff expression\n\
\ ^L            repaint the display if it gets corrupted\n\
\ ^R            repaint the display after resizing the window\n\
\ :source       look at the source-code application of this expression\n\
\ :Source       look at the source-code definition of current function\n\
\ :observe      use hat-observe to find all applications of this function\n\
\ :observe pat  use hat-observe to find all applications matching a pattern\n\
\ :location     use hat-observe to find all applications at this call site\n\
\ :trail        start a fresh hat-trail with the current expression\n\
\ :detect       use hat-detect to debug the current expression\n\
\ :animate      use hat-anim to animate reduction of the current expression\n\
\ :set          show all current mode settings\n\
\ :set <flag>   change one mode setting\n\
\   <flag> can be: uneval [on|off]      show unevaluated expressions in full\n\
\                  strSugar [on|off]    sugar character strings\n\
\                  listSugar [on|off]   sugar lists with [,,,] \n\
\                  equations [on|off]   show equations, not just redexes\n\
\                  qualified [on|off]   show module qualifiers on all names\n\
\                  cutoff <n>           cut-off depth for deeply nested exprs\n\
\ :+[n]         shortcut to increase cutoff depth\n\
\ :-[n]         shortcut to decrease cutoff depth\n\
\ :resize       repaint the display after resizing the window\n\
\ :help   :?    show this help text\n\
\ :quit         quit\n"

{-
\ SPACE         show one reduction step of whole expression\n\
-}

-- A 'more'-style viewer for text that is longer than the screen height.
moreViewer :: [String] -> State -> IO ()
moreViewer lines state =
  let maxsize = height state - startLine state in
  if length lines < maxsize
  then do putStr (goto 1 (startLine state) ++ clearDown)
          putStr (unlines lines)
          putStr (highlight [Bold] (replicate (width state - 1) '-'))
          _ <- getChar
          return ()
  else do let (top,rest) = splitAt (maxsize-1) lines
          putStr (goto 1 (startLine state) ++ clearDown)
          putStr (unlines top)
          putStr (highlight [Bold] (replicate (width state - 7) '-'
                                   ++ "more->"))
          _ <- getChar
          moreViewer rest state
