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
import PreludeBuiltinTypes
import DirectoryBuiltinTypes
import DirectoryBuiltin
import TimeBuiltin
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

foreign import haskell "Prelude.=="
 primPermissionsEq :: Permissions -> Permissions -> Bool

foreign import haskell "Prelude.compare"
 primPermissionsCompare :: Permissions -> Permissions -> Ordering

foreign import haskell "Prelude.<="
 primPermissionsLeEq :: Permissions -> Permissions -> Bool

foreign import haskell "Prelude.readsPrec"
 primPermissionsReadsPrec :: Int -> String -> [(Permissions,String)]

foreign import haskell "Prelude.showsPrec"
 primPermissionsShowsPrec :: Int -> Permissions -> String -> String


foreign import haskell "Directory.createDirectory"
 createDirectory  :: String -> IO ()
foreign import haskell "Directory.removeDirectory"
 removeDirectory  :: String -> IO ()
foreign import haskell "Directory.removeFile"
 removeFile  :: String -> IO ()
foreign import haskell "Directory.renameDirectory"
 renameDirectory  :: String -> String -> IO ()
foreign import haskell "Directory.renameFile"
 renameFile  :: String -> String -> IO ()

foreign import haskell "Directory.getDirectoryContents"
 getDirectoryContents  :: String -> IO [String]
foreign import haskell "Directory.getCurrentDirectory"
 getCurrentDirectory  :: IO String
foreign import haskell "Directory.setCurrentDirectory"
 setCurrentDirectory  :: String -> IO ()

foreign import haskell "Directory.doesFileExist"
 doesFileExist :: String -> IO Bool
foreign import haskell "Directory.doesDirectoryExist"
 doesDirectoryExist :: String -> IO Bool

foreign import haskell "Directory.getPermissions"
 getPermissions :: String -> IO Permissions
foreign import haskell "Directory.setPermissions"
 setPermissions :: String -> Permissions -> IO ()

-- Removed, because in base-4.6 this function has a different type.
-- foreign import haskell "Directory.getModificationTime"
--  getModificationTime :: String -> IO ClockTime