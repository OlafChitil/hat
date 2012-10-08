module Hat.DirectoryBuiltin where

import Hat.Hat as T
import Hat.PreludeBuiltinTypes
import Hat.DirectoryBuiltinTypes
import qualified System.Directory as Directory

toPermissions :: RefExp -> R Permissions -> Directory.Permissions
toPermissions h 
  (R (Permissions preadable pwritable pexecutable psearchable) _) =
    Directory.setOwnerReadable (toBool h preadable) $
    Directory.setOwnerWritable (toBool h pwritable) $
    Directory.setOwnerExecutable (toBool h pexecutable) $
    Directory.setOwnerSearchable (toBool h psearchable) $ 
    Directory.emptyPermissions

fromPermissions :: RefExp -> Directory.Permissions -> R Permissions
fromPermissions h permission =
    con4 mkNoSrcPos h Permissions aPermissions 
      (T.wrapForward h (fromBool h (Directory.readable permission))) 
      (T.wrapForward h (fromBool h (Directory.writable permission)))
      (T.wrapForward h (fromBool h (Directory.executable permission))) 
      (T.wrapForward h (fromBool h (Directory.searchable permission)))
