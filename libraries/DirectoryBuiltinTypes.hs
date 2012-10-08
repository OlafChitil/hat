module DirectoryBuiltinTypes where

data Permissions = Permissions {
 readable,   writable,
 executable, searchable :: Bool
    }
