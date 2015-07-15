-- A Haskell 98 module (not Haskell 2010)

module Directory ( 
     Permissions( Permissions, readable, writable, executable, searchable ), 
     createDirectory, removeDirectory, removeFile, 
     renameDirectory, renameFile, getDirectoryContents,
     getCurrentDirectory, setCurrentDirectory,
     doesFileExist, doesDirectoryExist,
     getPermissions, setPermissions --, getModificationTime 
     ) where

-- import Time ( ClockTime )
import PreludeBuiltinTypes as NotHat.T
import DirectoryBuiltinTypes as NotHat.T
import DirectoryBuiltin as NotHat.T
import TimeBuiltin as NotHat.T
import qualified NotHat.System.Directory as NotHat.Directory

instance Eq   Permissions where 
  (==) = primPermissionsEq

instance Ord  Permissions where 
  compare = primPermissionsCompare
  (<=) = primPermissionsLeEq

instance Read Permissions where 
  readsPrec = primPermissionsReadsPrec

instance Show Permissions where
  showsPrec = primPermissionsShowsPrec

foreign import ccall "NotHat.Prelude.=="
 primPermissionsEq :: Permissions -> Permissions -> Bool

foreign import ccall "NotHat.Prelude.compare"
 primPermissionsCompare :: Permissions -> Permissions -> Ordering

foreign import ccall "NotHat.Prelude.<="
 primPermissionsLeEq :: Permissions -> Permissions -> Bool

foreign import ccall "NotHat.Prelude.readsPrec"
 primPermissionsReadsPrec :: Int -> String -> [(Permissions,String)]

foreign import ccall "NotHat.Prelude.showsPrec"
 primPermissionsShowsPrec :: Int -> Permissions -> String -> String


foreign import ccall "NotHat.Directory.createDirectory"
 createDirectory  :: String -> IO ()
foreign import ccall "NotHat.Directory.removeDirectory"
 removeDirectory  :: String -> IO ()
foreign import ccall "NotHat.Directory.removeFile"
 removeFile  :: String -> IO ()
foreign import ccall "NotHat.Directory.renameDirectory"
 renameDirectory  :: String -> String -> IO ()
foreign import ccall "NotHat.Directory.renameFile"
 renameFile  :: String -> String -> IO ()

foreign import ccall "NotHat.Directory.getDirectoryContents"
 getDirectoryContents  :: String -> IO [String]
foreign import ccall "NotHat.Directory.getCurrentDirectory"
 getCurrentDirectory  :: IO String
foreign import ccall "NotHat.Directory.setCurrentDirectory"
 setCurrentDirectory  :: String -> IO ()

foreign import ccall "NotHat.Directory.doesFileExist"
 doesFileExist :: String -> IO Bool
foreign import ccall "NotHat.Directory.doesDirectoryExist"
 doesDirectoryExist :: String -> IO Bool

foreign import ccall "NotHat.Directory.getPermissions"
 getPermissions :: String -> IO Permissions
foreign import ccall "NotHat.Directory.setPermissions"
 setPermissions :: String -> Permissions -> IO ()

-- Removed, because in base-4.6 this function has a different type.
-- foreign import ccall "NotHat.Directory.getModificationTime"
--  getModificationTime :: String -> IO ClockTime