module IO (
     Handle, HandlePosn,
     IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
     BufferMode(NoBuffering,LineBuffering,BlockBuffering),
     SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
     stdin, stdout, stderr, 
     openFile, hClose, hFileSize, hIsEOF, isEOF,
     hSetBuffering, hGetBuffering, hFlush, 
     hGetPosn, hSetPosn, hSeek, 
     hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead, hGetContents, 
     hPutChar, hPutStr, hPutStrLn, hPrint,
     hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
     isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
     isFullError, isEOFError,
     isIllegalOperation, isPermissionError, isUserError, 
     ioeGetErrorString, ioeGetHandle, ioeGetFileName,
     try, bracket, bracket_,

     -- ...and what the Prelude exports
     IO, FilePath, IOError, ioError, userError, catch, interact,
     putChar, putStr, putStrLn, print, getChar, getLine, getContents,
     readFile, writeFile, appendFile, readIO, readLn
     ) where

import Ix(Ix(range, index, inRange))
import PreludeBuiltinTypes as NotHat.T
import NotHat.System.IO(Handle,HandlePosn)
import qualified NotHat.System.IO as NotHat.IO
import qualified NotHat.System.IO.Error as NotHat.Error
import IOBuiltinTypes
import IOBuiltin as NotHat.T

-- data Handle = ... -- implementation-dependent


foreign import ccall "NotHat.Prelude.=="
 primHandleEq :: Handle -> Handle -> Bool

foreign import ccall "NotHat.Prelude.showsPrec"
 primHandleShowsPrec :: Int -> Handle -> String -> String

instance Eq Handle where 
  (==) = primHandleEq

instance Show Handle where 
  showsPrec = primHandleShowsPrec

-- data HandlePosn = ... -- implementation-dependent

foreign import ccall "NotHat.Prelude.=="
 primHandlePosnEq :: HandlePosn -> HandlePosn -> Bool

instance Eq HandlePosn where 
  (==) = primHandlePosnEq

{- not implemented in GHC 5.02
foreign import ccall "NotHat.Prelude.showsPrec"
 primHandlePosnShowsPrec :: Int -> HandlePosn -> String -> String

instance Show HandlePosn where
  showsPrec = primHandlePosnShowsPrec
-}


foreign import ccall "NotHat.IO.stdin"
 stdin :: Handle
foreign import ccall "NotHat.IO.stdout"
 stdout :: Handle
foreign import ccall "NotHat.IO.stderr"
 stderr :: Handle

foreign import ccall "NotHat.IO.openFile"
 openFile              :: String -> IOMode -> IO Handle
foreign import ccall "NotHat.IO.hClose"
 hClose                :: Handle -> IO ()

foreign import ccall "NotHat.IO.hFileSize"
 hFileSize             :: Handle -> IO Integer
foreign import ccall "NotHat.IO.hIsEOF"
 hIsEOF                :: Handle -> IO Bool

isEOF                 :: IO Bool
isEOF                 =  hIsEOF stdin

foreign import ccall "NotHat.IO.hSetBuffering"
 hSetBuffering         :: Handle  -> BufferMode -> IO ()
foreign import ccall "NotHat.IO.hGetBuffering"
 hGetBuffering         :: Handle  -> IO BufferMode
foreign import ccall "NotHat.IO.hFlush"
 hFlush                :: Handle -> IO () 
foreign import ccall "NotHat.IO.hGetPosn"
 hGetPosn              :: Handle -> IO HandlePosn
foreign import ccall "NotHat.IO.hSetPosn"
 hSetPosn              :: HandlePosn -> IO () 
foreign import ccall "NotHat.IO.hSeek"
 hSeek                 :: Handle -> SeekMode -> Integer -> IO () 

foreign import ccall "NotHat.IO.hWaitForInput"
 hWaitForInput       :: Handle -> Int -> IO Bool

hReady :: Handle -> IO Bool 
hReady h = hWaitForInput h 0

foreign import ccall "NotHat.IO.hGetChar"
 hGetChar              :: Handle -> IO Char
foreign import ccall "NotHat.IO.hGetLine"
 hGetLine              :: Handle -> IO String
foreign import ccall "NotHat.IO.hLookAhead"
 hLookAhead            :: Handle -> IO Char
foreign import ccall "NotHat.IO.hGetContents"
 hGetContents          :: Handle -> IO String
foreign import ccall "NotHat.(\\_ h c -> T.outputTrace khPutChar [c] Prelude.>> IO.hPutChar h c) Prelude.True"
 hPutChar              :: Handle -> Char -> IO ()
foreign import ccall "NotHat.(\\_ h s -> T.outputTrace khPutStr s Prelude.>> IO.hPutStr h s) Prelude.True"
 hPutStr               :: Handle -> String -> IO () 

hPutStrLn         :: Handle -> String -> IO ()
hPutStrLn h s     =  do hPutStr h s
                        hPutStr h "\n"

hPrint     :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

foreign import ccall "NotHat.IO.hIsOpen"
 hIsOpen               :: Handle -> IO Bool
foreign import ccall "NotHat.IO.hIsClosed"
 hIsClosed             :: Handle -> IO Bool
foreign import ccall "NotHat.IO.hIsReadable"
 hIsReadable           :: Handle -> IO Bool
foreign import ccall "NotHat.IO.hIsWritable"
 hIsWritable           :: Handle -> IO Bool
foreign import ccall "NotHat.IO.hIsSeekable"
 hIsSeekable           :: Handle -> IO Bool

foreign import ccall "NotHat.Error.isAlreadyExistsError"
 isAlreadyExistsError  :: IOError -> Bool
foreign import ccall "NotHat.Error.isDoesNotExistError"
 isDoesNotExistError   :: IOError -> Bool 
foreign import ccall "NotHat.Error.isAlreadyInUseError"
 isAlreadyInUseError   :: IOError -> Bool
foreign import ccall "NotHat.Error.isFullError"
 isFullError           :: IOError -> Bool
foreign import ccall "NotHat.Error.isEOFError"
 isEOFError            :: IOError -> Bool
foreign import ccall "NotHat.Error.isIllegalOperation"
 isIllegalOperation    :: IOError -> Bool
foreign import ccall "NotHat.Error.isPermissionError"
 isPermissionError     :: IOError -> Bool
foreign import ccall "NotHat.Error.isUserError"
 isUserError           :: IOError -> Bool

foreign import ccall "NotHat.Error.ioeGetErrorString"
 ioeGetErrorString     :: IOError -> String
foreign import ccall "NotHat.Error.ioeGetHandle"
 ioeGetHandle          :: IOError -> Maybe Handle

foreign import ccall "NotHat.Error.ioeGetFileName"
 ioeGetFileName        :: IOError -> Maybe String

-- Just provide an implementation of the system-indendent
-- actions that IO exports.

try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e