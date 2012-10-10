module Hat.System.IO
  (IO(),gfixIO,afixIO,hfixIO,FilePath(),Handle(),gstdin,gstdout,gstderr
    ,gwithFile,awithFile,hwithFile,gopenFile,aopenFile,hopenFile,IOMode(ReadMode
      ,WriteMode,AppendMode,ReadWriteMode),aReadMode,aWriteMode,aAppendMode
    ,aReadWriteMode,ghClose,ahClose,hhClose,greadFile,areadFile,hreadFile
    ,gwriteFile,awriteFile,hwriteFile,gappendFile,aappendFile,happendFile
    ,ghFileSize,ahFileSize,hhFileSize,ghSetFileSize,ahSetFileSize,hhSetFileSize
    ,ghIsEOF,ahIsEOF,hhIsEOF,gisEOF,BufferMode(NoBuffering,LineBuffering
      ,BlockBuffering),aNoBuffering,aLineBuffering,aBlockBuffering
    ,ghSetBuffering,ahSetBuffering,hhSetBuffering,ghGetBuffering,ahGetBuffering
    ,hhGetBuffering,ghFlush,ahFlush,hhFlush,ghGetPosn,ahGetPosn,hhGetPosn
    ,ghSetPosn,ahSetPosn,hhSetPosn,HandlePosn(),ghSeek,ahSeek,hhSeek
    ,SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),aAbsoluteSeek,aRelativeSeek
    ,aSeekFromEnd,ghTell,ahTell,hhTell,ghIsOpen,ahIsOpen,hhIsOpen,ghIsClosed
    ,ahIsClosed,hhIsClosed,ghIsReadable,ahIsReadable,hhIsReadable,ghIsWritable
    ,ahIsWritable,hhIsWritable,ghIsSeekable,ahIsSeekable,hhIsSeekable
    ,ghIsTerminalDevice,ahIsTerminalDevice,hhIsTerminalDevice,ghSetEcho
    ,ahSetEcho,hhSetEcho,ghGetEcho,ahGetEcho,hhGetEcho,ghShow,ahShow,hhShow
    ,ghWaitForInput,ahWaitForInput,hhWaitForInput,ghReady,ahReady,hhReady
    ,ghGetChar,ahGetChar,hhGetChar,ghGetLine,ahGetLine,hhGetLine,ghLookAhead
    ,ahLookAhead,hhLookAhead,ghGetContents,ahGetContents,hhGetContents,ghPutChar
    ,ahPutChar,hhPutChar,ghPutStr,ahPutStr,hhPutStr,ghPutStrLn,ahPutStrLn
    ,hhPutStrLn,ghPrint,ahPrint,hhPrint,ginteract,ainteract,hinteract,gputChar
    ,aputChar,hputChar,gputStr,aputStr,hputStr,gputStrLn,aputStrLn,hputStrLn
    ,gprint,aprint,hprint,ggetChar,ggetLine,ggetContents,greadIO,areadIO,hreadIO
    ,greadLn) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.IO 
import Hat.PreludeBuiltinTypes 
import Hat.IOBuiltin 
import qualified System.IO 

gfixIO :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.Fun a (IO a)) (IO a))

gfixIO pfixIO p = T.ufun1 afixIO pfixIO p hfixIO

hfixIO z1fixIO kfixIO =
  (T.fromIO T.fromId) kfixIO
    (System.IO.fixIO ((toFun T.fromId (T.toIO T.toId)) kfixIO z1fixIO))

gwithFile ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun String (T.Fun IOMode (T.Fun (T.Fun Handle (IO r)) (IO r))))

gwithFile pwithFile p = T.ufun3 awithFile pwithFile p hwithFile

hwithFile z1withFile z2withFile z3withFile kwithFile =
  (T.fromIO T.fromId) kwithFile
    (System.IO.withFile (toString kwithFile z1withFile)
      (toIOMode kwithFile z2withFile)
      ((toFun fromHandle (T.toIO T.toId)) kwithFile z3withFile))

ghSetFileSize ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (T.Fun Integer (IO T.Tuple0)))

ghSetFileSize phSetFileSize p =
  T.ufun2 ahSetFileSize phSetFileSize p hhSetFileSize

hhSetFileSize z1hSetFileSize z2hSetFileSize khSetFileSize =
  (T.fromIO T.fromTuple0) khSetFileSize
    (System.IO.hSetFileSize (toHandle khSetFileSize z1hSetFileSize)
      (T.toInteger khSetFileSize z2hSetFileSize))

ghTell :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (IO Integer))

ghTell phTell p = T.ufun1 ahTell phTell p hhTell

hhTell z1hTell khTell =
  (T.fromIO T.fromInteger) khTell (System.IO.hTell (toHandle khTell z1hTell))

ghIsTerminalDevice :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (IO Bool))

ghIsTerminalDevice phIsTerminalDevice p =
  T.ufun1 ahIsTerminalDevice phIsTerminalDevice p hhIsTerminalDevice

hhIsTerminalDevice z1hIsTerminalDevice khIsTerminalDevice =
  (T.fromIO fromBool) khIsTerminalDevice
    (System.IO.hIsTerminalDevice
      (toHandle khIsTerminalDevice z1hIsTerminalDevice))

ghSetEcho ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (T.Fun Bool (IO T.Tuple0)))

ghSetEcho phSetEcho p = T.ufun2 ahSetEcho phSetEcho p hhSetEcho

hhSetEcho z1hSetEcho z2hSetEcho khSetEcho =
  (T.fromIO T.fromTuple0) khSetEcho
    (System.IO.hSetEcho (toHandle khSetEcho z1hSetEcho)
      (toBool khSetEcho z2hSetEcho))

ghGetEcho :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (IO Bool))

ghGetEcho phGetEcho p = T.ufun1 ahGetEcho phGetEcho p hhGetEcho

hhGetEcho z1hGetEcho khGetEcho =
  (T.fromIO fromBool) khGetEcho
    (System.IO.hGetEcho (toHandle khGetEcho z1hGetEcho))

ghShow :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Handle (IO String))

ghShow phShow p = T.ufun1 ahShow phShow p hhShow

hhShow z1hShow khShow =
  (T.fromIO fromString) khShow (System.IO.hShow (toHandle khShow z1hShow))

tSystem_IO = T.mkModule "System.IO" "System/IO.hs" Prelude.False

afixIO = T.mkVariable tSystem_IO 200001 210028 3 1 "fixIO" Prelude.False

awithFile = T.mkVariable tSystem_IO 230001 240056 3 3 "withFile" Prelude.False

ahSetFileSize =
  T.mkVariable tSystem_IO 260001 270041 3 2 "hSetFileSize" Prelude.False

ahTell = T.mkVariable tSystem_IO 290001 300023 3 1 "hTell" Prelude.False

ahIsTerminalDevice =
  T.mkVariable tSystem_IO 320001 330035 3 1 "hIsTerminalDevice" Prelude.False

ahSetEcho = T.mkVariable tSystem_IO 350001 360034 3 2 "hSetEcho" Prelude.False

ahGetEcho = T.mkVariable tSystem_IO 380001 390026 3 1 "hGetEcho" Prelude.False

ahShow = T.mkVariable tSystem_IO 410001 420023 3 1 "hShow" Prelude.False
