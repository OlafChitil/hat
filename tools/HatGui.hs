module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import HatStack
import HatCover

import Maybe
import Monad
import System

import qualified SExp (SExp(..),Label,fileNode2SExp,sExp2Doc,prune)
import PrettyLibHighlight (Doc,pretty,nest,text,(<>),parens)


data HatGui = HatGui {
    wndMain :: Window,
    cmdFilename :: Button,
    lblFilename :: Label,
    
    tvStack :: TreeView,
    lblStack :: Label,
    tvStackData :: TreeStore,
    
    txtCover :: TextView,
    txtCoverData :: TextBuffer
    }


main :: IO ()
main = do
    initGUI

    -- load up our main window
    dialogXmlM <- xmlNew "HatGui.glade"
    let dialogXml = case dialogXmlM of
            (Just dialogXml) -> dialogXml
            Nothing -> error "Can't find the glade file \"HatGui.glade\""

    -- get a handle on a various objects from the glade file
    wndMain <- xmlGetWidget dialogXml castToWindow "wndMain"
    cmdFilename <- xmlGetWidget dialogXml castToButton "cmdFilename"
    lblFilename <- xmlGetWidget dialogXml castToLabel "lblFilename"
    tvStack <- xmlGetWidget dialogXml castToTreeView "tvStack"
    lblStack <- xmlGetWidget dialogXml castToLabel "lblStack"
    tvStackData <- treeStoreNew [TMstring, TMstring]
    txtCover <- xmlGetWidget dialogXml castToTextView "txtCover"
    txtCoverData <- textViewGetBuffer txtCover
    let hatGui = HatGui
                    wndMain cmdFilename lblFilename
                    tvStack lblStack tvStackData
                    txtCover txtCoverData

    wndMain `onDestroy` mainQuit
    cmdFilename `onClicked` openOpenFileDialog hatGui

    -- prepare hat-stack
    colPosition <- treeViewColumnNew
    renderer <- cellRendererTextNew
    treeViewColumnPackStart colPosition renderer True
    treeViewColumnSetTitle colPosition "Position"
    treeViewColumnAddAttribute colPosition renderer "text" 0
    treeViewAppendColumn tvStack colPosition

    colExpression <- treeViewColumnNew
    renderer <- cellRendererTextNew
    treeViewColumnPackStart colExpression renderer True
    treeViewColumnSetTitle colExpression "Expression"
    treeViewColumnAddAttribute colExpression renderer "text" 1
    treeViewAppendColumn tvStack colExpression

    treeViewSetModel tvStack tvStackData
    
    -- prepare hat-cover
    tags <- textBufferGetTagTable txtCoverData
    tagFilename <- textTagNew "filename"
    tagHighlight <- textTagNew "highlight"
    tagNormal <- textTagNew "normal"
    textTagTableAdd tags tagFilename
    textTagTableAdd tags tagHighlight
    textTagTableAdd tags tagNormal
    set tagFilename [textTagPriority := 2, textTagFamily := "Sans", textTagWeight := 700]
    set tagHighlight [textTagBackground := "#ff9", textTagPriority := 1, textTagFamily := "Monospace"]
    set tagNormal [textTagBackground := "white", textTagPriority := 0, textTagFamily := "Monospace"]


    args <- getArgs
    when (not $ null args) $
        selectHatFile hatGui (head args)

    -- The final step is to display the main window and run the main loop
    widgetShowAll wndMain
    mainGUI


openOpenFileDialog :: HatGui -> IO ()
openOpenFileDialog hatGui@HatGui{wndMain=wndMain, lblFilename=lblFilename} = do
    dialog <- fileChooserDialogNew
              (Just "Select the .hat file to open")
              (Just wndMain)
              FileChooserActionOpen                         --the kind of dialog we want
              [("gtk-cancel",ResponseCancel),("gtk-open",ResponseAccept)]
            
    filt <- fileFilterNew
    fileFilterSetName filt "Hat Files (*.hat)"
    fileFilterAddPattern filt "*.hat"
    fileChooserAddFilter dialog filt

    widgetShow dialog
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             selectHatFile hatGui fileName
        _ -> return ()
    widgetHide dialog


selectHatFile :: HatGui -> FilePath -> IO ()
selectHatFile hatGui@HatGui{lblFilename=lblFilename} hatfile = do
    set lblFilename [labelText := "Opened: " ++ hatfile]
    displayStack hatGui hatfile
    displayCover hatGui hatfile


displayStack :: HatGui -> FilePath -> IO ()
displayStack hatGui@HatGui{tvStack=tvStack, lblStack=lblStack, tvStackData=tvStackData} hatfile = do
    res <- hatStack hatfile
    treeStoreClear tvStackData
    if isNothing res then do
        set lblStack [labelText := "The program did not crash"]
     else do
        let Just (crsh, stck) = res
        set lblStack [labelText := crsh]
        mapM_ f stck
    where
        f (a,b) = do
            let aa = pretty 80 $ SExp.sExp2Doc False True False (\_->id) (SExp.prune 10 a)
            let bb = case b of {Nothing -> "unknown"; Just (mod,line) -> mod ++ ":" ++ show line}
            next <- treeStoreAppend tvStackData Nothing
            treeStoreSetValue tvStackData next 0 (GVstring $ Just bb)
            treeStoreSetValue tvStackData next 1 (GVstring $ Just aa)
  

displayCover :: HatGui -> FilePath -> IO ()
displayCover hatGui@HatGui{txtCover=txtCover, txtCoverData=txtCoverData} hatfile = do
        tags <- textBufferGetTagTable txtCoverData
        Just tagFilename <- textTagTableLookup tags "filename"
        Just tagHighlight <- textTagTableLookup tags "highlight"
        Just tagNormal <- textTagTableLookup tags "normal"
        
        res <- hatCover hatfile []
        textBufferSetText txtCoverData ""
        
        start <- textBufferGetStartIter txtCoverData
        mark <- textBufferCreateMark txtCoverData Nothing start True
        end <- textBufferGetEndIter txtCoverData
        
        mapM_ (f (applyTextTag mark end) tagFilename tagHighlight tagNormal) res
    where
        f app tagFilename tagHighlight tagNormal (file,pos) = do
            app tagFilename $ "-- file " ++ file ++ " --\n"
            printModuleCover (app tagHighlight, app tagNormal) (file,pos)

        applyTextTag mark end tag text = do
            textBufferInsert txtCoverData end text
            imark <- textBufferGetIterAtMark txtCoverData mark
            textBufferApplyTag txtCoverData tag imark end
            textBufferMoveMark txtCoverData mark end
            

---------------------------------------------------------------------
-- HAT COVER STUFF
-- mainly copied directly (should be shared somehow...)

type Out = String -> IO ()


expand :: String -> String
expand = expandFrom 1

expandFrom :: Int -> String -> String
expandFrom _ "" = ""
expandFrom n (x:xs) = f (expandFrom (n+d) xs)
  where
  (d, f) = if x=='\t' then (8 - (n-1)`mod`8, (take d spaces ++))
                      else (1, (x:))

spaces :: String
spaces = repeat ' '


line :: Out -> Out
line f x = f x >> f "\n"

printModuleCover :: (Out, Out) -> (String, [Interval LC]) -> IO ()
printModuleCover hiOnOff (f, c) =
  do
    src <- readFile f
    printLo hiOnOff (1,1) c (map expand (lines src))

printLo :: (Out, Out) -> LC -> [Interval LC] -> [String] -> IO ()
printLo (hi,low) _      [] srcLines =
  mapM_ (line low) srcLines
printLo (hi,low) (lineNo,colNo) (((lstart,cstart),(lstop,cstop)):ivals) srcLines =
  do
    mapM_ (line low) (take lnLo srcLines)
    low (take chLo (head srcLines'))
    printHi (hi,low)
      (lstart,cstart) (lstop,cstop) ivals
      (drop chLo (head srcLines') : tail srcLines')
  where
  lnLo = lstart-lineNo
  chLo = cstart-(if lnLo==0 then colNo else 1)
  srcLines' = drop lnLo srcLines 
  
printHi :: (Out, Out) -> LC -> LC -> [Interval LC] -> [String] -> IO ()
printHi hiOnOff (lineNo,colNo) (lstop,cstop) ivals srcLines =
  do
    mapM_ (line (highOut hiOnOff)) (take lnHi srcLines)
    highOut hiOnOff (take chHi (head srcLines'))
    printLo hiOnOff
      (lstop,cstop+1) ivals
      (drop chHi (head srcLines') : tail srcLines')    
  where
  lnHi = lstop-lineNo
  chHi = 1+cstop-(if lnHi==0 then colNo else 1)
  srcLines' = drop lnHi srcLines
  
highOut :: (Out, Out) -> String -> IO ()
highOut (hiOn, hiOff) s = do
    hiOff $ takeWhile (==' ') s
    hiOn $ dropWhile (== ' ') s


