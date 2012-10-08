module Hat.Directory
  (Permissions(Permissions,breadable,bwritable,bexecutable,bsearchable)
    ,greadable,gwritable,gexecutable,gsearchable,hreadable,hwritable,hexecutable
    ,hsearchable,areadable,awritable,aexecutable,asearchable,aPermissions
    ,gcreateDirectory,acreateDirectory,hcreateDirectory,gremoveDirectory
    ,aremoveDirectory,hremoveDirectory,gremoveFile,aremoveFile,hremoveFile
    ,grenameDirectory,arenameDirectory,hrenameDirectory,grenameFile,arenameFile
    ,hrenameFile,ggetDirectoryContents,agetDirectoryContents
    ,hgetDirectoryContents,ggetCurrentDirectory,gsetCurrentDirectory
    ,asetCurrentDirectory,hsetCurrentDirectory,gdoesFileExist,adoesFileExist
    ,hdoesFileExist,gdoesDirectoryExist,adoesDirectoryExist,hdoesDirectoryExist
    ,ggetPermissions,agetPermissions,hgetPermissions,gsetPermissions
    ,asetPermissions,hsetPermissions,ggetModificationTime,agetModificationTime
    ,hgetModificationTime) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.Time  (ClockTime())
import Hat.PreludeBuiltinTypes 
import Hat.DirectoryBuiltinTypes 
import Hat.DirectoryBuiltin 
import Hat.TimeBuiltin 
import qualified System.Directory as Directory 

instance Eq (Permissions)
  where
  
  (!==) (%==) p = T.uconstUse (%==) p (|==)
  
  (|==) =
    T.uconstDef T.mkRoot (+#@=&=#@=$+==)
      (\ p -> gprimPermissionsEq T.mkNoSrcPos p)
  

instance Ord (Permissions)
  where
  
  gcompare pcompare p = T.uconstUse pcompare p scompare
  
  scompare =
    T.uconstDef T.mkRoot a21v3v21v34compare
      (\ p -> gprimPermissionsCompare T.mkNoSrcPos p)
  
  (!<=) (%<=) p = T.uconstUse (%<=) p (|<=)
  
  (|<=) =
    T.uconstDef T.mkRoot (+$$=&=$$=$@<=)
      (\ p -> gprimPermissionsLeEq T.mkNoSrcPos p)
  

instance Read (Permissions)
  where
  
  greadsPrec preadsPrec p = T.uconstUse preadsPrec p sreadsPrec
  
  sreadsPrec =
    T.uconstDef T.mkRoot a25v3v25v38readsPrec
      (\ p -> gprimPermissionsReadsPrec T.mkNoSrcPos p)
  

instance Show (Permissions)
  where
  
  gshowsPrec pshowsPrec p = T.uconstUse pshowsPrec p sshowsPrec
  
  sshowsPrec =
    T.uconstDef T.mkRoot a28v3v28v38showsPrec
      (\ p -> gprimPermissionsShowsPrec T.mkNoSrcPos p)
  

gprimPermissionsEq ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Permissions (T.Fun Permissions Bool))

gprimPermissionsEq pprimPermissionsEq p =
  T.ufun2 aprimPermissionsEq pprimPermissionsEq p hprimPermissionsEq

hprimPermissionsEq z1primPermissionsEq z2primPermissionsEq kprimPermissionsEq =
  fromBool kprimPermissionsEq
    ((toPermissions kprimPermissionsEq z1primPermissionsEq)
      Prelude.==
      (toPermissions kprimPermissionsEq z2primPermissionsEq))

gprimPermissionsCompare ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Permissions (T.Fun Permissions Ordering))

gprimPermissionsCompare pprimPermissionsCompare p =
  T.ufun2 aprimPermissionsCompare pprimPermissionsCompare p
    hprimPermissionsCompare

hprimPermissionsCompare z1primPermissionsCompare z2primPermissionsCompare
  kprimPermissionsCompare =
  fromOrdering kprimPermissionsCompare
    (Prelude.compare
      (toPermissions kprimPermissionsCompare z1primPermissionsCompare)
      (toPermissions kprimPermissionsCompare z2primPermissionsCompare))

gprimPermissionsLeEq ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Permissions (T.Fun Permissions Bool))

gprimPermissionsLeEq pprimPermissionsLeEq p =
  T.ufun2 aprimPermissionsLeEq pprimPermissionsLeEq p hprimPermissionsLeEq

hprimPermissionsLeEq z1primPermissionsLeEq z2primPermissionsLeEq
  kprimPermissionsLeEq =
  fromBool kprimPermissionsLeEq
    ((toPermissions kprimPermissionsLeEq z1primPermissionsLeEq)
      Prelude.<=
      (toPermissions kprimPermissionsLeEq z2primPermissionsLeEq))

gprimPermissionsReadsPrec ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun Int (T.Fun String (T.List (T.Tuple2 Permissions String))))

gprimPermissionsReadsPrec pprimPermissionsReadsPrec p =
  T.ufun2 aprimPermissionsReadsPrec pprimPermissionsReadsPrec p
    hprimPermissionsReadsPrec

hprimPermissionsReadsPrec z1primPermissionsReadsPrec z2primPermissionsReadsPrec
  kprimPermissionsReadsPrec =
  (fromList (T.fromTuple2 fromPermissions fromString)) kprimPermissionsReadsPrec
    (Prelude.readsPrec
      (T.toInt kprimPermissionsReadsPrec z1primPermissionsReadsPrec)
      (toString kprimPermissionsReadsPrec z2primPermissionsReadsPrec))

gprimPermissionsShowsPrec ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Int (T.Fun Permissions (T.Fun String String)))

gprimPermissionsShowsPrec pprimPermissionsShowsPrec p =
  T.ufun3 aprimPermissionsShowsPrec pprimPermissionsShowsPrec p
    hprimPermissionsShowsPrec

hprimPermissionsShowsPrec z1primPermissionsShowsPrec z2primPermissionsShowsPrec
  z3primPermissionsShowsPrec kprimPermissionsShowsPrec =
  fromString kprimPermissionsShowsPrec
    (Prelude.showsPrec
      (T.toInt kprimPermissionsShowsPrec z1primPermissionsShowsPrec)
      (toPermissions kprimPermissionsShowsPrec z2primPermissionsShowsPrec)
      (toString kprimPermissionsShowsPrec z3primPermissionsShowsPrec))

gcreateDirectory :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO T.Tuple0))

gcreateDirectory pcreateDirectory p =
  T.ufun1 acreateDirectory pcreateDirectory p hcreateDirectory

hcreateDirectory z1createDirectory kcreateDirectory =
  (T.fromIO T.fromTuple0) kcreateDirectory
    (Directory.createDirectory (toString kcreateDirectory z1createDirectory))

gremoveDirectory :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO T.Tuple0))

gremoveDirectory premoveDirectory p =
  T.ufun1 aremoveDirectory premoveDirectory p hremoveDirectory

hremoveDirectory z1removeDirectory kremoveDirectory =
  (T.fromIO T.fromTuple0) kremoveDirectory
    (Directory.removeDirectory (toString kremoveDirectory z1removeDirectory))

gremoveFile :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO T.Tuple0))

gremoveFile premoveFile p = T.ufun1 aremoveFile premoveFile p hremoveFile

hremoveFile z1removeFile kremoveFile =
  (T.fromIO T.fromTuple0) kremoveFile
    (Directory.removeFile (toString kremoveFile z1removeFile))

grenameDirectory ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (T.Fun String (IO T.Tuple0)))

grenameDirectory prenameDirectory p =
  T.ufun2 arenameDirectory prenameDirectory p hrenameDirectory

hrenameDirectory z1renameDirectory z2renameDirectory krenameDirectory =
  (T.fromIO T.fromTuple0) krenameDirectory
    (Directory.renameDirectory (toString krenameDirectory z1renameDirectory)
      (toString krenameDirectory z2renameDirectory))

grenameFile ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (T.Fun String (IO T.Tuple0)))

grenameFile prenameFile p = T.ufun2 arenameFile prenameFile p hrenameFile

hrenameFile z1renameFile z2renameFile krenameFile =
  (T.fromIO T.fromTuple0) krenameFile
    (Directory.renameFile (toString krenameFile z1renameFile)
      (toString krenameFile z2renameFile))

ggetDirectoryContents ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO (T.List String)))

ggetDirectoryContents pgetDirectoryContents p =
  T.ufun1 agetDirectoryContents pgetDirectoryContents p hgetDirectoryContents

hgetDirectoryContents z1getDirectoryContents kgetDirectoryContents =
  (T.fromIO (fromList fromString)) kgetDirectoryContents
    (Directory.getDirectoryContents
      (toString kgetDirectoryContents z1getDirectoryContents))

ggetCurrentDirectory :: T.RefSrcPos -> T.RefExp -> T.R (IO String)

ggetCurrentDirectory pgetCurrentDirectory p =
  T.uconstUse pgetCurrentDirectory p sgetCurrentDirectory

sgetCurrentDirectory =
  T.uconstDef T.mkRoot agetCurrentDirectory
    (\ p -> (T.fromIO fromString) p Directory.getCurrentDirectory)

gsetCurrentDirectory ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO T.Tuple0))

gsetCurrentDirectory psetCurrentDirectory p =
  T.ufun1 asetCurrentDirectory psetCurrentDirectory p hsetCurrentDirectory

hsetCurrentDirectory z1setCurrentDirectory ksetCurrentDirectory =
  (T.fromIO T.fromTuple0) ksetCurrentDirectory
    (Directory.setCurrentDirectory
      (toString ksetCurrentDirectory z1setCurrentDirectory))

gdoesFileExist :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO Bool))

gdoesFileExist pdoesFileExist p =
  T.ufun1 adoesFileExist pdoesFileExist p hdoesFileExist

hdoesFileExist z1doesFileExist kdoesFileExist =
  (T.fromIO fromBool) kdoesFileExist
    (Directory.doesFileExist (toString kdoesFileExist z1doesFileExist))

gdoesDirectoryExist :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO Bool))

gdoesDirectoryExist pdoesDirectoryExist p =
  T.ufun1 adoesDirectoryExist pdoesDirectoryExist p hdoesDirectoryExist

hdoesDirectoryExist z1doesDirectoryExist kdoesDirectoryExist =
  (T.fromIO fromBool) kdoesDirectoryExist
    (Directory.doesDirectoryExist
      (toString kdoesDirectoryExist z1doesDirectoryExist))

ggetPermissions ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO Permissions))

ggetPermissions pgetPermissions p =
  T.ufun1 agetPermissions pgetPermissions p hgetPermissions

hgetPermissions z1getPermissions kgetPermissions =
  (T.fromIO fromPermissions) kgetPermissions
    (Directory.getPermissions (toString kgetPermissions z1getPermissions))

gsetPermissions ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun String (T.Fun Permissions (IO T.Tuple0)))

gsetPermissions psetPermissions p =
  T.ufun2 asetPermissions psetPermissions p hsetPermissions

hsetPermissions z1setPermissions z2setPermissions ksetPermissions =
  (T.fromIO T.fromTuple0) ksetPermissions
    (Directory.setPermissions (toString ksetPermissions z1setPermissions)
      (toPermissions ksetPermissions z2setPermissions))

ggetModificationTime ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (IO ClockTime))

ggetModificationTime pgetModificationTime p =
  T.ufun1 agetModificationTime pgetModificationTime p hgetModificationTime

hgetModificationTime z1getModificationTime kgetModificationTime =
  (T.fromIO fromClockTime) kgetModificationTime
    (Directory.getModificationTime
      (toString kgetModificationTime z1getModificationTime))

tDirectory = T.mkModule "Directory" "Directory.hs" Prelude.False

aprimPermissionsEq =
  T.mkVariable tDirectory 300001 310056 3 2 "primPermissionsEq" Prelude.False

aprimPermissionsCompare =
  T.mkVariable tDirectory 330001 340065 3 2 "primPermissionsCompare"
    Prelude.False

aprimPermissionsLeEq =
  T.mkVariable tDirectory 360001 370058 3 2 "primPermissionsLeEq" Prelude.False

aprimPermissionsReadsPrec =
  T.mkVariable tDirectory 390001 400047 3 2 "primPermissionsReadsPrec"
    Prelude.False

aprimPermissionsShowsPrec =
  T.mkVariable tDirectory 420001 430067 3 3 "primPermissionsShowsPrec"
    Prelude.False

acreateDirectory =
  T.mkVariable tDirectory 460001 470033 3 1 "createDirectory" Prelude.False

aremoveDirectory =
  T.mkVariable tDirectory 480001 490033 3 1 "removeDirectory" Prelude.False

aremoveFile =
  T.mkVariable tDirectory 500001 510028 3 1 "removeFile" Prelude.False

arenameDirectory =
  T.mkVariable tDirectory 520001 530043 3 2 "renameDirectory" Prelude.False

arenameFile =
  T.mkVariable tDirectory 540001 550038 3 2 "renameFile" Prelude.False

agetDirectoryContents =
  T.mkVariable tDirectory 570001 580038 3 1 "getDirectoryContents" Prelude.False

agetCurrentDirectory =
  T.mkVariable tDirectory 590001 600027 3 0 "getCurrentDirectory" Prelude.False

asetCurrentDirectory =
  T.mkVariable tDirectory 610001 620037 3 1 "setCurrentDirectory" Prelude.False

adoesFileExist =
  T.mkVariable tDirectory 640001 650030 3 1 "doesFileExist" Prelude.False

adoesDirectoryExist =
  T.mkVariable tDirectory 660001 670035 3 1 "doesDirectoryExist" Prelude.False

agetPermissions =
  T.mkVariable tDirectory 690001 700031 3 1 "getPermissions" Prelude.False

asetPermissions =
  T.mkVariable tDirectory 710001 720046 3 2 "setPermissions" Prelude.False

agetModificationTime =
  T.mkVariable tDirectory 740001 750036 3 1 "getModificationTime" Prelude.False

(+#@=&=#@=$+==) = T.mkVariable tDirectory 180004 180026 16 0 "==" Prelude.False

a21v3v21v34compare =
  T.mkVariable tDirectory 210003 210034 3 0 "compare" Prelude.False

(+$$=&=$$=$@<=) = T.mkVariable tDirectory 220004 220028 16 0 "<=" Prelude.False

a25v3v25v38readsPrec =
  T.mkVariable tDirectory 250003 250038 3 0 "readsPrec" Prelude.False

a28v3v28v38showsPrec =
  T.mkVariable tDirectory 280003 280038 3 0 "showsPrec" Prelude.False
