module Hat.Directory
       (Permissions(Permissions, breadable, bwritable,
                    bexecutable, bsearchable),
        greadable, gwritable, gexecutable, gsearchable,
        hreadable, hwritable, hexecutable, hsearchable,
        areadable, awritable, aexecutable, asearchable,
        aPermissions, gcreateDirectory, acreateDirectory,
        hcreateDirectory, gremoveDirectory, aremoveDirectory,
        hremoveDirectory, gremoveFile, aremoveFile,
        hremoveFile, grenameDirectory, arenameDirectory,
        hrenameDirectory, grenameFile, arenameFile,
        hrenameFile, ggetDirectoryContents,
        agetDirectoryContents, hgetDirectoryContents,
        ggetCurrentDirectory, gsetCurrentDirectory,
        asetCurrentDirectory, hsetCurrentDirectory,
        gdoesFileExist, adoesFileExist, hdoesFileExist,
        gdoesDirectoryExist, adoesDirectoryExist,
        hdoesDirectoryExist, ggetPermissions,
        agetPermissions, hgetPermissions, gsetPermissions,
        asetPermissions, hsetPermissions)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes as T
import Hat.DirectoryBuiltinTypes as T
import Hat.DirectoryBuiltin as T
import Hat.TimeBuiltin as T
import qualified System.Directory as Directory
 
instance Eq Permissions where
        (%==) !== p = T.uconstUse (%==) p (|==)
        (|==)
          = T.uconstDef p (+$!=%=$!=$+==)
              (\ p -> gprimPermissionsEq T.mkNoSrcPos p)
 
instance Ord Permissions where
        gcompare pcompare p = T.uconstUse pcompare p scompare
        scompare
          = T.uconstDef p c23v3v23v34compare
              (\ p -> gprimPermissionsCompare T.mkNoSrcPos p)
        (%<=) !<= p = T.uconstUse (%<=) p (|<=)
        (|<=)
          = T.uconstDef p (+$&=%=$&=$@<=)
              (\ p -> gprimPermissionsLeEq T.mkNoSrcPos p)
 
instance Read Permissions where
        greadsPrec preadsPrec p
          = T.uconstUse preadsPrec p sreadsPrec
        sreadsPrec
          = T.uconstDef p c27v3v27v38readsPrec
              (\ p -> gprimPermissionsReadsPrec T.mkNoSrcPos p)
 
instance Show Permissions where
        gshowsPrec pshowsPrec p
          = T.uconstUse pshowsPrec p sshowsPrec
        sshowsPrec
          = T.uconstDef p c30v3v30v38showsPrec
              (\ p -> gprimPermissionsShowsPrec T.mkNoSrcPos p)
 
gprimPermissionsEq ::
                   T.RefSrcPos ->
                     T.RefExp ->
                       T.R (T.Fun Permissions (T.Fun Permissions Bool))
gprimPermissionsEq pprimPermissionsEq p
  = T.ufun2 aprimPermissionsEq pprimPermissionsEq p
      hprimPermissionsEq
hprimPermissionsEq z1primPermissionsEq
  z2primPermissionsEq kprimPermissionsEq
  = T.fromBool kprimPermissionsEq
      ((Prelude.==)
         (T.toPermissions kprimPermissionsEq
            z1primPermissionsEq)
         (T.toPermissions kprimPermissionsEq
            z2primPermissionsEq))
 
gprimPermissionsCompare ::
                        T.RefSrcPos ->
                          T.RefExp ->
                            T.R (T.Fun Permissions (T.Fun Permissions Ordering))
gprimPermissionsCompare pprimPermissionsCompare p
  = T.ufun2 aprimPermissionsCompare
      pprimPermissionsCompare
      p
      hprimPermissionsCompare
hprimPermissionsCompare z1primPermissionsCompare
  z2primPermissionsCompare kprimPermissionsCompare
  = T.fromOrdering kprimPermissionsCompare
      (Prelude.compare
         (T.toPermissions kprimPermissionsCompare
            z1primPermissionsCompare)
         (T.toPermissions kprimPermissionsCompare
            z2primPermissionsCompare))
 
gprimPermissionsLeEq ::
                     T.RefSrcPos ->
                       T.RefExp ->
                         T.R (T.Fun Permissions (T.Fun Permissions Bool))
gprimPermissionsLeEq pprimPermissionsLeEq p
  = T.ufun2 aprimPermissionsLeEq pprimPermissionsLeEq p
      hprimPermissionsLeEq
hprimPermissionsLeEq z1primPermissionsLeEq
  z2primPermissionsLeEq kprimPermissionsLeEq
  = T.fromBool kprimPermissionsLeEq
      ((Prelude.<=)
         (T.toPermissions kprimPermissionsLeEq
            z1primPermissionsLeEq)
         (T.toPermissions kprimPermissionsLeEq
            z2primPermissionsLeEq))
 
gprimPermissionsReadsPrec ::
                          T.RefSrcPos ->
                            T.RefExp ->
                              T.R
                                (T.Fun Int
                                   (T.Fun String
                                      (T.List (T.Tuple2 Permissions String))))
gprimPermissionsReadsPrec pprimPermissionsReadsPrec p
  = T.ufun2 aprimPermissionsReadsPrec
      pprimPermissionsReadsPrec
      p
      hprimPermissionsReadsPrec
hprimPermissionsReadsPrec z1primPermissionsReadsPrec
  z2primPermissionsReadsPrec kprimPermissionsReadsPrec
  = T.fromList
      (T.fromTuple2 T.fromPermissions T.fromString)
      kprimPermissionsReadsPrec
      (Prelude.readsPrec
         (T.toInt kprimPermissionsReadsPrec
            z1primPermissionsReadsPrec)
         (T.toString kprimPermissionsReadsPrec
            z2primPermissionsReadsPrec))
 
gprimPermissionsShowsPrec ::
                          T.RefSrcPos ->
                            T.RefExp ->
                              T.R
                                (T.Fun Int
                                   (T.Fun Permissions (T.Fun String String)))
gprimPermissionsShowsPrec pprimPermissionsShowsPrec p
  = T.ufun3 aprimPermissionsShowsPrec
      pprimPermissionsShowsPrec
      p
      hprimPermissionsShowsPrec
hprimPermissionsShowsPrec z1primPermissionsShowsPrec
  z2primPermissionsShowsPrec z3primPermissionsShowsPrec
  kprimPermissionsShowsPrec
  = T.fromString kprimPermissionsShowsPrec
      (Prelude.showsPrec
         (T.toInt kprimPermissionsShowsPrec
            z1primPermissionsShowsPrec)
         (T.toPermissions kprimPermissionsShowsPrec
            z2primPermissionsShowsPrec)
         (T.toString kprimPermissionsShowsPrec
            z3primPermissionsShowsPrec))
 
gcreateDirectory ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun String (IO T.Tuple0))
gcreateDirectory pcreateDirectory p
  = T.ufun1 acreateDirectory pcreateDirectory p
      hcreateDirectory
hcreateDirectory z1createDirectory kcreateDirectory
  = T.fromIO T.fromTuple0 kcreateDirectory
      (Directory.createDirectory
         (T.toString kcreateDirectory z1createDirectory))
 
gremoveDirectory ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun String (IO T.Tuple0))
gremoveDirectory premoveDirectory p
  = T.ufun1 aremoveDirectory premoveDirectory p
      hremoveDirectory
hremoveDirectory z1removeDirectory kremoveDirectory
  = T.fromIO T.fromTuple0 kremoveDirectory
      (Directory.removeDirectory
         (T.toString kremoveDirectory z1removeDirectory))
 
gremoveFile ::
            T.RefSrcPos ->
              T.RefExp -> T.R (T.Fun String (IO T.Tuple0))
gremoveFile premoveFile p
  = T.ufun1 aremoveFile premoveFile p hremoveFile
hremoveFile z1removeFile kremoveFile
  = T.fromIO T.fromTuple0 kremoveFile
      (Directory.removeFile
         (T.toString kremoveFile z1removeFile))
 
grenameDirectory ::
                 T.RefSrcPos ->
                   T.RefExp ->
                     T.R (T.Fun String (T.Fun String (IO T.Tuple0)))
grenameDirectory prenameDirectory p
  = T.ufun2 arenameDirectory prenameDirectory p
      hrenameDirectory
hrenameDirectory z1renameDirectory z2renameDirectory
  krenameDirectory
  = T.fromIO T.fromTuple0 krenameDirectory
      (Directory.renameDirectory
         (T.toString krenameDirectory z1renameDirectory)
         (T.toString krenameDirectory z2renameDirectory))
 
grenameFile ::
            T.RefSrcPos ->
              T.RefExp ->
                T.R (T.Fun String (T.Fun String (IO T.Tuple0)))
grenameFile prenameFile p
  = T.ufun2 arenameFile prenameFile p hrenameFile
hrenameFile z1renameFile z2renameFile krenameFile
  = T.fromIO T.fromTuple0 krenameFile
      (Directory.renameFile
         (T.toString krenameFile z1renameFile)
         (T.toString krenameFile z2renameFile))
 
ggetDirectoryContents ::
                      T.RefSrcPos ->
                        T.RefExp -> T.R (T.Fun String (IO (T.List String)))
ggetDirectoryContents pgetDirectoryContents p
  = T.ufun1 agetDirectoryContents pgetDirectoryContents
      p
      hgetDirectoryContents
hgetDirectoryContents z1getDirectoryContents
  kgetDirectoryContents
  = T.fromIO (T.fromList T.fromString)
      kgetDirectoryContents
      (Directory.getDirectoryContents
         (T.toString kgetDirectoryContents
            z1getDirectoryContents))
 
ggetCurrentDirectory ::
                     T.RefSrcPos -> T.RefExp -> T.R (IO String)
ggetCurrentDirectory pgetCurrentDirectory p
  = T.uconstUse pgetCurrentDirectory p
      sgetCurrentDirectory
sgetCurrentDirectory
  = T.uconstDef p agetCurrentDirectory
      (\ p ->
         T.fromIO T.fromString p
           Directory.getCurrentDirectory)
 
gsetCurrentDirectory ::
                     T.RefSrcPos ->
                       T.RefExp -> T.R (T.Fun String (IO T.Tuple0))
gsetCurrentDirectory psetCurrentDirectory p
  = T.ufun1 asetCurrentDirectory psetCurrentDirectory p
      hsetCurrentDirectory
hsetCurrentDirectory z1setCurrentDirectory
  ksetCurrentDirectory
  = T.fromIO T.fromTuple0 ksetCurrentDirectory
      (Directory.setCurrentDirectory
         (T.toString ksetCurrentDirectory
            z1setCurrentDirectory))
 
gdoesFileExist ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun String (IO Bool))
gdoesFileExist pdoesFileExist p
  = T.ufun1 adoesFileExist pdoesFileExist p
      hdoesFileExist
hdoesFileExist z1doesFileExist kdoesFileExist
  = T.fromIO T.fromBool kdoesFileExist
      (Directory.doesFileExist
         (T.toString kdoesFileExist z1doesFileExist))
 
gdoesDirectoryExist ::
                    T.RefSrcPos ->
                      T.RefExp -> T.R (T.Fun String (IO Bool))
gdoesDirectoryExist pdoesDirectoryExist p
  = T.ufun1 adoesDirectoryExist pdoesDirectoryExist p
      hdoesDirectoryExist
hdoesDirectoryExist z1doesDirectoryExist
  kdoesDirectoryExist
  = T.fromIO T.fromBool kdoesDirectoryExist
      (Directory.doesDirectoryExist
         (T.toString kdoesDirectoryExist
            z1doesDirectoryExist))
 
ggetPermissions ::
                T.RefSrcPos ->
                  T.RefExp -> T.R (T.Fun String (IO Permissions))
ggetPermissions pgetPermissions p
  = T.ufun1 agetPermissions pgetPermissions p
      hgetPermissions
hgetPermissions z1getPermissions kgetPermissions
  = T.fromIO T.fromPermissions kgetPermissions
      (Directory.getPermissions
         (T.toString kgetPermissions z1getPermissions))
 
gsetPermissions ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R (T.Fun String (T.Fun Permissions (IO T.Tuple0)))
gsetPermissions psetPermissions p
  = T.ufun2 asetPermissions psetPermissions p
      hsetPermissions
hsetPermissions z1setPermissions z2setPermissions
  ksetPermissions
  = T.fromIO T.fromTuple0 ksetPermissions
      (Directory.setPermissions
         (T.toString ksetPermissions z1setPermissions)
         (T.toPermissions ksetPermissions z2setPermissions))
acreateDirectory
  = T.mkVariable tDirectory 480001 490020 3 (1)
      "createDirectory"
      Prelude.False
adoesDirectoryExist
  = T.mkVariable tDirectory 680001 690022 3 (1)
      "doesDirectoryExist"
      Prelude.False
adoesFileExist
  = T.mkVariable tDirectory 660001 670017 3 (1)
      "doesFileExist"
      Prelude.False
agetCurrentDirectory
  = T.mkVariable tDirectory 610001 620024 3 (0)
      "getCurrentDirectory"
      Prelude.False
agetDirectoryContents
  = T.mkVariable tDirectory 590001 600025 3 (1)
      "getDirectoryContents"
      Prelude.False
agetPermissions
  = T.mkVariable tDirectory 710001 720018 3 (1)
      "getPermissions"
      Prelude.False
aprimPermissionsCompare
  = T.mkVariable tDirectory 350001 360026 3 (2)
      "primPermissionsCompare"
      Prelude.False
aprimPermissionsEq
  = T.mkVariable tDirectory 320001 330021 3 (2)
      "primPermissionsEq"
      Prelude.False
aprimPermissionsLeEq
  = T.mkVariable tDirectory 380001 390023 3 (2)
      "primPermissionsLeEq"
      Prelude.False
aprimPermissionsReadsPrec
  = T.mkVariable tDirectory 410001 420028 3 (2)
      "primPermissionsReadsPrec"
      Prelude.False
aprimPermissionsShowsPrec
  = T.mkVariable tDirectory 440001 450028 3 (3)
      "primPermissionsShowsPrec"
      Prelude.False
aremoveDirectory
  = T.mkVariable tDirectory 500001 510020 3 (1)
      "removeDirectory"
      Prelude.False
aremoveFile
  = T.mkVariable tDirectory 520001 530015 3 (1)
      "removeFile"
      Prelude.False
arenameDirectory
  = T.mkVariable tDirectory 540001 550020 3 (2)
      "renameDirectory"
      Prelude.False
arenameFile
  = T.mkVariable tDirectory 560001 570015 3 (2)
      "renameFile"
      Prelude.False
asetCurrentDirectory
  = T.mkVariable tDirectory 630001 640024 3 (1)
      "setCurrentDirectory"
      Prelude.False
asetPermissions
  = T.mkVariable tDirectory 730001 740018 3 (2)
      "setPermissions"
      Prelude.False
(+$!=%=$!=$+==)
  = T.mkVariable tDirectory 200003 200026 3 (-1) "=="
      Prelude.False
c23v3v23v34compare
  = T.mkVariable tDirectory 230003 230034 3 (-1)
      "compare"
      Prelude.False
(+$&=%=$&=$@<=)
  = T.mkVariable tDirectory 240003 240028 3 (-1) "<="
      Prelude.False
c27v3v27v38readsPrec
  = T.mkVariable tDirectory 270003 270038 3 (-1)
      "readsPrec"
      Prelude.False
c30v3v30v38showsPrec
  = T.mkVariable tDirectory 300003 300038 3 (-1)
      "showsPrec"
      Prelude.False
p = T.mkRoot
tDirectory
  = T.mkModule "Directory" "Directory.hs" Prelude.False