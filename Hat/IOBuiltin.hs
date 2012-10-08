module Hat.IOBuiltin where

import qualified System.IO as IO
import System.IO(Handle,HandlePosn)
import qualified Prelude
import Hat.Hat as T 
import Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.IOBuiltinTypes

toHandle :: RefExp -> R Handle -> IO.Handle
toHandle h (R e _) = e

fromHandle :: RefExp -> IO.Handle -> R Handle
fromHandle h e = R e (T.mkValueUse h mkNoSrcPos aHandle)

aHandle :: RefAtom
aHandle = mkAbstract "Handle"

toHandlePosn :: RefExp -> R HandlePosn -> IO.HandlePosn
toHandlePosn h (R e _) = e

fromHandlePosn :: RefExp -> IO.HandlePosn -> R HandlePosn
fromHandlePosn h p = R p (T.mkValueUse h mkNoSrcPos aHandlePosn)

aHandlePosn :: RefAtom
aHandlePosn = mkAbstract "HandlePosn"

fromMaybe :: (RefExp -> a -> R b) -> RefExp -> Prelude.Maybe a -> R (Maybe b)
fromMaybe f h Prelude.Nothing = T.con0 mkNoSrcPos h Nothing aNothing
fromMaybe f h (Prelude.Just arg) = 
  T.con1 mkNoSrcPos h Just aJust (T.wrapForward h (f h arg))

toMaybe :: (RefExp -> R a -> b) -> RefExp -> R (Maybe a) -> Prelude.Maybe b
toMaybe f h (R Nothing _) = Prelude.Nothing
toMaybe f h (R (Just x) _) = Prelude.Just (f h x)

toIOMode :: RefExp -> R IOMode -> IO.IOMode
toIOMode h (R ReadMode _) = IO.ReadMode
toIOMode h (R WriteMode _) = IO.WriteMode
toIOMode h (R AppendMode _) = IO.AppendMode
toIOMode h (R ReadWriteMode _) = IO.ReadWriteMode

toBufferMode :: RefExp -> R BufferMode -> IO.BufferMode
toBufferMode h (R (BlockBuffering maybeInt) _) = 
  IO.BlockBuffering (toMaybe toInt h maybeInt)
toBufferMode h (R NoBuffering _) = IO.NoBuffering
toBufferMode h (R LineBuffering _) = IO.LineBuffering

fromBufferMode :: RefExp -> IO.BufferMode -> R BufferMode
fromBufferMode h (IO.BlockBuffering maybeInt) = 
  T.con1 mkNoSrcPos h BlockBuffering aBlockBuffering 
    (T.wrapForward h (fromMaybe fromInt h maybeInt))
fromBufferMode h IO.NoBuffering = 
  T.con0 mkNoSrcPos h NoBuffering aNoBuffering
fromBufferMode h IO.LineBuffering = 
  T.con0 mkNoSrcPos h LineBuffering aLineBuffering

toSeekMode :: RefExp -> R SeekMode -> IO.SeekMode
toSeekMode h (R AbsoluteSeek _) = IO.AbsoluteSeek
toSeekMode h (R RelativeSeek _) = IO.RelativeSeek
toSeekMode h (R SeekFromEnd _) = IO.SeekFromEnd

