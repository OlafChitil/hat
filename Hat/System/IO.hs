module Hat.System.IO
       (IO, gfixIO, afixIO, hfixIO, FilePath, Handle,
        gstdin, gstdout, gstderr, gwithFile, awithFile,
        hwithFile, gopenFile, aopenFile, hopenFile,
        IOMode(ReadMode, WriteMode, AppendMode,
               ReadWriteMode),
        aReadMode, aWriteMode, aAppendMode, aReadWriteMode,
        ghClose, ahClose, hhClose, greadFile, areadFile,
        hreadFile, gwriteFile, awriteFile, hwriteFile,
        gappendFile, aappendFile, happendFile, ghFileSize,
        ahFileSize, hhFileSize, ghSetFileSize, ahSetFileSize,
        hhSetFileSize, ghIsEOF, ahIsEOF, hhIsEOF, gisEOF,
        BufferMode(NoBuffering, LineBuffering,
                   BlockBuffering),
        aNoBuffering, aLineBuffering, aBlockBuffering,
        ghSetBuffering, ahSetBuffering, hhSetBuffering,
        ghGetBuffering, ahGetBuffering, hhGetBuffering,
        ghFlush, ahFlush, hhFlush, ghGetPosn, ahGetPosn,
        hhGetPosn, ghSetPosn, ahSetPosn, hhSetPosn,
        HandlePosn, ghSeek, ahSeek, hhSeek,
        SeekMode(AbsoluteSeek, RelativeSeek, SeekFromEnd),
        aAbsoluteSeek, aRelativeSeek, aSeekFromEnd, ghTell,
        ahTell, hhTell, ghIsOpen, ahIsOpen, hhIsOpen,
        ghIsClosed, ahIsClosed, hhIsClosed, ghIsReadable,
        ahIsReadable, hhIsReadable, ghIsWritable,
        ahIsWritable, hhIsWritable, ghIsSeekable,
        ahIsSeekable, hhIsSeekable, ghIsTerminalDevice,
        ahIsTerminalDevice, hhIsTerminalDevice, ghSetEcho,
        ahSetEcho, hhSetEcho, ghGetEcho, ahGetEcho,
        hhGetEcho, ghShow, ahShow, hhShow, ghWaitForInput,
        ahWaitForInput, hhWaitForInput, ghReady, ahReady,
        hhReady, ghGetChar, ahGetChar, hhGetChar, ghGetLine,
        ahGetLine, hhGetLine, ghLookAhead, ahLookAhead,
        hhLookAhead, ghGetContents, ahGetContents,
        hhGetContents, ghPutChar, ahPutChar, hhPutChar,
        ghPutStr, ahPutStr, hhPutStr, ghPutStrLn, ahPutStrLn,
        hhPutStrLn, ghPrint, ahPrint, hhPrint, ginteract,
        ainteract, hinteract, gputChar, aputChar, hputChar,
        gputStr, aputStr, hputStr, gputStrLn, aputStrLn,
        hputStrLn, gprint, aprint, hprint, ggetChar,
        ggetLine, ggetContents, greadIO, areadIO, hreadIO,
        greadLn)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.IO
import Hat.PreludeBuiltinTypes as T
import Hat.IOBuiltin as T
import qualified System.IO
 
gfixIO ::
       T.RefSrcPos ->
         T.RefExp -> T.R (T.Fun (T.Fun a (IO a)) (IO a))
gfixIO pfixIO p = T.ufun1 afixIO pfixIO p hfixIO
hfixIO z1fixIO kfixIO
  = T.fromIO T.fromId kfixIO
      (System.IO.fixIO
         ((T.toFun T.fromId (T.toIO T.toId)) kfixIO z1fixIO))
 
gwithFile ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun String
                   (T.Fun IOMode (T.Fun (T.Fun Handle (IO r)) (IO r))))
gwithFile pwithFile p
  = T.ufun3 awithFile pwithFile p hwithFile
hwithFile z1withFile z2withFile z3withFile kwithFile
  = T.fromIO T.fromId kwithFile
      (System.IO.withFile (T.toString kwithFile z1withFile)
         (T.toIOMode kwithFile z2withFile)
         ((T.toFun T.fromHandle (T.toIO T.toId)) kwithFile
            z3withFile))
 
ghSetFileSize ::
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun Handle (T.Fun Integer (IO T.Tuple0)))
ghSetFileSize phSetFileSize p
  = T.ufun2 ahSetFileSize phSetFileSize p hhSetFileSize
hhSetFileSize z1hSetFileSize z2hSetFileSize
  khSetFileSize
  = T.fromIO T.fromTuple0 khSetFileSize
      (System.IO.hSetFileSize
         (T.toHandle khSetFileSize z1hSetFileSize)
         (T.toInteger khSetFileSize z2hSetFileSize))
 
ghTell ::
       T.RefSrcPos ->
         T.RefExp -> T.R (T.Fun Handle (IO Integer))
ghTell phTell p = T.ufun1 ahTell phTell p hhTell
hhTell z1hTell khTell
  = T.fromIO T.fromInteger khTell
      (System.IO.hTell (T.toHandle khTell z1hTell))
 
ghIsTerminalDevice ::
                   T.RefSrcPos ->
                     T.RefExp -> T.R (T.Fun Handle (IO Bool))
ghIsTerminalDevice phIsTerminalDevice p
  = T.ufun1 ahIsTerminalDevice phIsTerminalDevice p
      hhIsTerminalDevice
hhIsTerminalDevice z1hIsTerminalDevice
  khIsTerminalDevice
  = T.fromIO T.fromBool khIsTerminalDevice
      (System.IO.hIsTerminalDevice
         (T.toHandle khIsTerminalDevice z1hIsTerminalDevice))
 
ghSetEcho ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R (T.Fun Handle (T.Fun Bool (IO T.Tuple0)))
ghSetEcho phSetEcho p
  = T.ufun2 ahSetEcho phSetEcho p hhSetEcho
hhSetEcho z1hSetEcho z2hSetEcho khSetEcho
  = T.fromIO T.fromTuple0 khSetEcho
      (System.IO.hSetEcho (T.toHandle khSetEcho z1hSetEcho)
         (T.toBool khSetEcho z2hSetEcho))
 
ghGetEcho ::
          T.RefSrcPos ->
            T.RefExp -> T.R (T.Fun Handle (IO Bool))
ghGetEcho phGetEcho p
  = T.ufun1 ahGetEcho phGetEcho p hhGetEcho
hhGetEcho z1hGetEcho khGetEcho
  = T.fromIO T.fromBool khGetEcho
      (System.IO.hGetEcho
         (T.toHandle khGetEcho z1hGetEcho))
 
ghShow ::
       T.RefSrcPos ->
         T.RefExp -> T.R (T.Fun Handle (IO String))
ghShow phShow p = T.ufun1 ahShow phShow p hhShow
hhShow z1hShow khShow
  = T.fromIO T.fromString khShow
      (System.IO.hShow (T.toHandle khShow z1hShow))
afixIO
  = T.mkVariable tIO 200001 210010 3 (1) "fixIO"
      Prelude.False
ahGetEcho
  = T.mkVariable tIO 380001 390013 3 (1) "hGetEcho"
      Prelude.False
ahIsTerminalDevice
  = T.mkVariable tIO 320001 330022 3 (1)
      "hIsTerminalDevice"
      Prelude.False
ahSetEcho
  = T.mkVariable tIO 350001 360013 3 (2) "hSetEcho"
      Prelude.False
ahSetFileSize
  = T.mkVariable tIO 260001 270017 3 (2) "hSetFileSize"
      Prelude.False
ahShow
  = T.mkVariable tIO 410001 420010 3 (1) "hShow"
      Prelude.False
ahTell
  = T.mkVariable tIO 290001 300010 3 (1) "hTell"
      Prelude.False
awithFile
  = T.mkVariable tIO 230001 240013 3 (3) "withFile"
      Prelude.False
p = T.mkRoot
tIO
  = T.mkModule "System.IO" "System/IO.hs" Prelude.False