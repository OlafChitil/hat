module System.IO (  
    IO,  fixIO,  FilePath,  Handle,  stdin,  stdout,  stderr,  withFile,  
    openFile,  IOMode(ReadMode, WriteMode, AppendMode, ReadWriteMode),  hClose,  
    readFile,  writeFile,  appendFile,  hFileSize,  hSetFileSize,  hIsEOF,  
    isEOF,  BufferMode(NoBuffering, LineBuffering, BlockBuffering),  
    hSetBuffering,  hGetBuffering,  hFlush,  hGetPosn,  hSetPosn,  HandlePosn,  
    hSeek,  SeekMode(AbsoluteSeek, RelativeSeek, SeekFromEnd),  hTell,  
    hIsOpen,  hIsClosed,  hIsReadable,  hIsWritable,  hIsSeekable,  
    hIsTerminalDevice,  hSetEcho,  hGetEcho,  hShow,  hWaitForInput,  hReady,  
    hGetChar,  hGetLine,  hLookAhead,  hGetContents,  hPutChar,  hPutStr,  
    hPutStrLn,  hPrint,  interact,  putChar,  putStr,  putStrLn,  print,  
    getChar,  getLine,  getContents,  readIO,  readLn  
  ) where

import IO
import PreludeBuiltinTypes
import IOBuiltin
import qualified NotHat.System.IO

foreign import haskell "System.IO.fixIO"
  fixIO :: (a -> IO a) -> IO a

foreign import haskell "System.IO.withFile"
  withFile :: String -> IOMode -> (Handle -> IO r) -> IO r

foreign import haskell "System.IO.hSetFileSize"
  hSetFileSize :: Handle -> Integer -> IO ()

foreign import haskell "System.IO.hTell"
  hTell :: Handle -> IO Integer

foreign import haskell "System.IO.hIsTerminalDevice"
  hIsTerminalDevice :: Handle -> IO Bool

foreign import haskell "System.IO.hSetEcho"
  hSetEcho :: Handle -> Bool -> IO ()

foreign import haskell "System.IO.hGetEcho"
  hGetEcho :: Handle -> IO Bool

foreign import haskell "System.IO.hShow"
  hShow :: Handle -> IO String