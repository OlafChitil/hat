module Hat.DirectoryBuiltinTypes
       (Permissions(..), greadable, hreadable, gwritable,
        hwritable, gexecutable, hexecutable, gsearchable,
        hsearchable, aPermissions, aexecutable, areadable,
        asearchable, awritable)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
 
data Permissions = Permissions{breadable, bwritable,
                               bexecutable, bsearchable :: T.R Bool}
 
instance T.WrapVal Permissions where
        wrapVal pwrapVal
          kwrapVal@(Permissions (T.R _ z1wrapVal)
                      (T.R _ z2wrapVal) (T.R _ z3wrapVal)
                      (T.R _ z4wrapVal))
          p
          = T.R kwrapVal
              (T.mkValueApp4 p pwrapVal aPermissions z1wrapVal
                 z2wrapVal
                 z3wrapVal
                 z4wrapVal)
greadable preadable p
  = T.ufun1 areadable preadable p hreadable
hreadable (T.R z1readable _) p
  = T.projection T.mkNoSrcPos p (breadable z1readable)
gwritable pwritable p
  = T.ufun1 awritable pwritable p hwritable
hwritable (T.R z1writable _) p
  = T.projection T.mkNoSrcPos p (bwritable z1writable)
gexecutable pexecutable p
  = T.ufun1 aexecutable pexecutable p hexecutable
hexecutable (T.R z1executable _) p
  = T.projection T.mkNoSrcPos p
      (bexecutable z1executable)
gsearchable psearchable p
  = T.ufun1 asearchable psearchable p hsearchable
hsearchable (T.R z1searchable _) p
  = T.projection T.mkNoSrcPos p
      (bsearchable z1searchable)
aPermissions
  = T.mkConstructorWFields tDirectoryBuiltinTypes 30020
      30030
      3
      (4)
      "Permissions"
      ((:) areadable
         ((:) awritable
            ((:) aexecutable ((:) asearchable []))))
aexecutable
  = T.mkVariable tDirectoryBuiltinTypes 50002 50011 3
      (1)
      "executable"
      Prelude.False
areadable
  = T.mkVariable tDirectoryBuiltinTypes 40002 40009 3
      (1)
      "readable"
      Prelude.False
asearchable
  = T.mkVariable tDirectoryBuiltinTypes 50014 50023 3
      (1)
      "searchable"
      Prelude.False
awritable
  = T.mkVariable tDirectoryBuiltinTypes 40014 40021 3
      (1)
      "writable"
      Prelude.False
p = T.mkRoot
tDirectoryBuiltinTypes
  = T.mkModule "DirectoryBuiltinTypes"
      "DirectoryBuiltinTypes.hs"
      Prelude.False