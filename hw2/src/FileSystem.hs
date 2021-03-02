{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystem where

import Control.Exception (SomeException (..), catch)
import Control.Monad.Except (foldM, throwError, when)
import Control.Monad.Loops (untilJust)
import Data.Algorithm.Diff (Diff (..), getGroupedDiff)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (length, null, readFile, writeFile)
import qualified Data.ByteString.UTF8 as B8 (fromString, toString)
import Data.Either (isRight, partitionEithers)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map (delete, elems, empty, fromList, insert, lookup)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Time.Clock (UTCTime (..))
import System.Directory as Dir (Permissions (..), canonicalizePath, createDirectoryIfMissing,
                                doesDirectoryExist, emptyPermissions, getAccessTime, getFileSize,
                                getModificationTime, getPermissions, listDirectory,
                                removePathForcibly)
import System.FilePath (dropTrailingPathSeparator, equalFilePath, isAbsolute, makeRelative,
                        splitPath, takeBaseName, takeDirectory, takeExtension, takeFileName, (<.>),
                        (</>))

import Utils

-- |vcs version
data Version = Version { vmessage :: String
                       , vcontent :: ByteString
                       } deriving ( Show, Read )

-- |vcs
newtype Vcs = Vcs { vcsversions :: [Version] } deriving ( Show, Read )

-- |data to save/read vcs in/from file
data VcsSave = VcsSave [(FilePath, Vcs)] deriving ( Show, Read )

-- |filesystem entity: directory or file
data FSEntity = FSDirectory Directory
              | FSFile File
              deriving ( Show, Read )

-- |get path
getPath :: FSEntity -> FilePath
getPath (FSDirectory dir) = dpath dir
getPath (FSFile file)     = fpath file

-- |get permissions
getPermissions :: FSEntity -> Permissions
getPermissions (FSDirectory dir) = dpermissions dir
getPermissions (FSFile file)     = fpermissions file

-- |get last access time
getAccess :: FSEntity -> Maybe UTCTime
getAccess (FSDirectory dir) = daccess dir
getAccess (FSFile file)     = faccess file

-- |get last modification time
getModification :: FSEntity -> Maybe UTCTime
getModification (FSDirectory dir) = dmodification dir
getModification (FSFile file)     = fmodification file

instance FSEntityClass FSEntity where
  displayName (FSDirectory dir) = displayName dir
  displayName (FSFile file)     = displayName file

  treePrint i (FSDirectory dir) = treePrint i dir
  treePrint i (FSFile file)     = treePrint i file

  toFSEntity = id

-- |indent to print tree
getIndent :: String
getIndent = "--"

-- |print indent n times
printIndent :: Int -> String
printIndent indent = mconcat (replicate indent getIndent)

-- |class for some common functions
class FSEntityClass a where
  -- |print entity name
  displayName :: a -> String
  -- |print entity tree
  treePrint :: Int -> a -> String
  -- |convert to FSEntity
  toFSEntity :: a -> FSEntity

-- |data representation of file
data File = File { fpath               :: FilePath
                 , fcontent            :: ByteString
                 , fpermissions        :: Permissions
                 , faccess             :: Maybe UTCTime
                 , fmodification       :: Maybe UTCTime
                 , fsizeIfUnaccessible :: Maybe Int
                 , fvcs                :: Maybe Vcs
                 } deriving ( Show, Read )

instance FSEntityClass File where
  displayName :: File -> String
  displayName = takeFileName . fpath

  treePrint :: Int -> File -> String
  treePrint indent f = printIndent indent ++ displayName f

  toFSEntity :: File -> FSEntity
  toFSEntity f = FSFile f

-- |data representation of directory
data Directory = Directory { dpath         :: FilePath
                           , children      :: Map FilePath FSEntity
                           , dpermissions  :: Permissions
                           , daccess       :: Maybe UTCTime
                           , dmodification :: Maybe UTCTime
                           } deriving ( Show, Read )

instance FSEntityClass Directory where
  displayName :: Directory -> String
  displayName = takeBaseName . dpath

  treePrint :: Int -> Directory -> String
  treePrint indent dir =
    printIndent indent ++
    displayName dir ++
    if ((readable $ dpermissions dir) && (searchable $ dpermissions dir))
    then
      (if (null $ children dir) then "" else "\n") ++
      (intercalate "\n" $ map (treePrint (indent + 1)) (Map.elems $ children dir))
    else
      " (Can't access directory)"

  toFSEntity :: Directory -> FSEntity
  toFSEntity dir = FSDirectory dir

-- |save 'fs' to real fs
syncWithFs :: Directory -> [FilePath] -> IO ()
syncWithFs fs rms = do
  syncFiles (FSDirectory fs)
  syncRemoves rms
  saveVcs fs

-- |sync files
syncFiles :: FSEntity -> IO ()
syncFiles (FSDirectory dir) = do
  createDirectoryIfMissing True (dpath dir)
  mapM_ syncFiles (children dir)
syncFiles (FSFile file) = do
  when (writable $ fpermissions file) $
    B.writeFile (fpath file) (fcontent file)

-- |sync removed files
syncRemoves :: [FilePath] -> IO ()
syncRemoves rms = mapM_ removePathForcibly rms

-- |collect all vcs-s from all files in one data to save
collectVcs :: Directory -> VcsSave
collectVcs fs = do
  let files = listFilesEntities (FSDirectory fs)
  VcsSave $ map (\file -> (fpath file, fromJust $ fvcs file)) $ filter (isJust . fvcs) files

-- |set vcs-s to theit files by filepathes
setVcsAll :: Directory -> [(FilePath, Vcs)] -> (Directory, [FSError])
setVcsAll fs ((path, vcs) : rest) = do
  let eitherNewFs = setVcs fs vcs path
  case eitherNewFs of
    Right newFs -> do
      setVcsAll newFs rest
    Left err -> do
      let (fsWithVcsRest, errorsRest) = setVcsAll fs rest
      (fsWithVcsRest, err : errorsRest)
setVcsAll fs [] = (fs, [])

-- |filename to save/load vcs to on sync
vcsFileName :: String
vcsFileName = ".vcs"

-- |save vcs to file
saveVcs :: Directory -> IO ()
saveVcs fs = do
  let vcsSave = collectVcs fs
  writeFile (dpath fs </> vcsFileName) (show vcsSave)

-- |load vcs from file
loadVcs :: Directory -> IO (Directory, [FSError])
loadVcs fs = do
  VcsSave vcss <- readFile (dpath fs </> vcsFileName) >>= return . (read :: String -> VcsSave)
  return $ setVcsAll fs vcss

-- |copy real fs to object
copyFs :: FilePath -> IO FSEntity
copyFs path = do
  isDir <- doesDirectoryExist path
  pathCanonicalized <- canonicalizePath path
  permissions <- Dir.getPermissions path
  accessTime <- getAccessTime path
  modificationTime <- getModificationTime path

  if isDir
  then do
    if (readable permissions && searchable permissions)
    then do
      pathChildren <- listDirectory path
      let pathChildrenWithoutVcs = filter (/= vcsFileName) pathChildren
      childrenFs <- mapM (copyFs . (path </>)) pathChildrenWithoutVcs
      return $ FSDirectory $ Directory pathCanonicalized (Map.fromList $ zip pathChildrenWithoutVcs childrenFs) permissions (Just accessTime) (Just modificationTime)
    else do
      return $ FSDirectory $ Directory pathCanonicalized Map.empty permissions (Just accessTime) (Just modificationTime)
  else do
    fileContent <- B.readFile path `catch` (\(_ :: SomeException) -> pure "")
    fileSize <- getFileSize path
    let fileSizeMaybe = if (B.null fileContent && fileSize > 0) then (Just $ fromInteger fileSize) else Nothing
    return $ FSFile $ File pathCanonicalized fileContent permissions (Just accessTime) (Just modificationTime) (fileSizeMaybe) Nothing

-- |go from dir by relative path
goFromDirRelative :: Directory -> FilePath -> Maybe FSEntity
goFromDirRelative dir path =
  goPath (FSDirectory dir) $ splittedPath
    where
      splittedPath =
        if (path == ".")
        then []
        else map dropTrailingPathSeparator $ splitPath path

-- |go path relative by segments
goPath :: FSEntity -> [FilePath] -> Maybe FSEntity
goPath (FSDirectory dir) (cur : rest) = do
  next <- Map.lookup cur (children dir)
  goPath next rest
goPath (FSFile _) (_ : _) = Nothing
goPath fs [] = pure fs

-- |lists directory, if given path is notdirectory returns empty list
listDirPath :: Directory -> FilePath -> [String]
listDirPath fs path = fromMaybe [] $ do
  dir <- getDirectory fs path
  return $ listDir dir

-- |list directory as entity
listDir :: Directory -> [String]
listDir dir = map displayName $ Map.elems (children dir)

-- |list all files in entity as (name, path) pairs
-- Directory -> all children
-- File -> self
listDirFiles :: FSEntity -> [(String, FilePath)]
listDirFiles (FSDirectory dir)    = concatMap listDirFiles (Map.elems $ children dir)
listDirFiles fsfile@(FSFile file) = [(displayName fsfile, fpath file)]

-- |lists all files in entity
listFilesEntities :: FSEntity -> [File]
listFilesEntities (FSDirectory dir) = concatMap listFilesEntities (Map.elems $ children dir)
listFilesEntities (FSFile file)     = [file]

-- |get size of entity in bytes, result may be wrong if some
-- are unreadable or unavaliable for another reason
getSize :: FSEntity -> Int
getSize (FSFile file) = if (isJust (fsizeIfUnaccessible file)) then (fromJust (fsizeIfUnaccessible file)) else (B.length $ fcontent file)
getSize (FSDirectory dir) = foldr (\ch acc -> acc + (getSize ch)) 0 (children dir)

-- |is given path a direcory
isDirectory :: Directory -> FilePath -> Bool
isDirectory dir path = isJust $ getDirectory dir path

-- |gets directory if given path is a directory, else nothing
getDirectory :: Directory -> FilePath -> Maybe Directory
getDirectory fs path = do
  entity <- getEntity fs path
  case entity of
    (FSDirectory dir) -> return dir
    _                 -> Nothing

-- |get entity by path
getEntityEither :: Directory -> FilePath -> Either FSError FSEntity
getEntityEither fs path = do
  let entityMaybe = getEntity fs path

  if (isJust entityMaybe)
  then do
    return $ fromJust entityMaybe
  else do
    throwError $ NoSuchPathError path

-- |get file by path
getFileEither :: Directory -> FilePath -> Either FSError File
getFileEither fs path = do
  entity <- getEntityEither fs path

  case entity of
    (FSFile file) -> return file
    _             -> throwError (NotFileError path)

-- |get entity but result is packed in maybe
getEntity :: Directory -> FilePath -> Maybe FSEntity
getEntity fs path = do
  let relativePath =
        if (isAbsolute path)
        then makeRelative (dpath fs) path
        else path
  goFromDirRelative fs relativePath

-- |is given path a file
isFile :: Directory -> FilePath -> Bool
isFile dir path = isJust $ getFile dir path

-- |get file maybe
getFile :: Directory -> FilePath -> Maybe File
getFile fs path = do
  entity <- getEntity fs path
  case entity of
    (FSFile file) -> return file
    _             -> Nothing

-- |readable && writable permissions
rwPermissions :: Permissions
rwPermissions = emptyPermissions { readable = True, writable =  True }

-- |full permissions
fullPermissions :: Permissions
fullPermissions = rwPermissions { searchable = True, executable = True }

-- |error that can happen during evaluation
data FSError = ModifyingNonWritableError FilePath
             | NoSuchPathError FilePath
             | NotFileError FilePath
             | NotDirectoryError FilePath
             | NoVcsError FilePath
             | NoSuchVersionError Int
             | NotReadableError FilePath
             | NotWritableError FilePath
             | NotSearchableError FilePath
             | FileAlreadyExistError FilePath
             | DirectoryAlreadyExistError FilePath

instance Show FSError where
  show :: FSError -> String
  show (NoSuchPathError path)            = "given path: " ++ path ++ " is not a file or directory"
  show (NoVcsError path)                 = "no vcs in given file: " ++ path
  show (NotFileError path)               = "given path: " ++ path ++ " is not a file"
  show (NotDirectoryError path)          = "given path: " ++ path ++ " is not a directory"
  show (FileAlreadyExistError path)      = "file:  " ++ path ++ " already exist"
  show (DirectoryAlreadyExistError path) = "directory:  " ++ path ++ " already exist"
  show (NoSuchVersionError index)        = "no version with index: " ++ (show index)
  show (ModifyingNonWritableError path)  = "can't modify non writable directory/file: " ++ path
  show (NotReadableError path)           = "file/directory: " ++ path ++ " is not readable"
  show (NotWritableError path)           = "file/directory: " ++ path ++ " is not writable"
  show (NotSearchableError path)         = "file/directory: " ++ path ++ " is not searchable"

-- |insert entity in given directory by path segments
insertEntitySegments :: Directory -> [FilePath] -> FSEntity -> Either FSError Directory
insertEntitySegments (Directory p ch permissions acc md) (cur : rest) e = do
  let old = Map.lookup cur ch
      (FSDirectory oldFromJust) =
        if isJust old
        then fromJust old
        else FSDirectory $ Directory (p </> cur) Map.empty (rwPermissions { executable = True, searchable = True }) Nothing Nothing

  if (not (writable permissions) && isNothing old)
  then do
    throwError $ ModifyingNonWritableError p
  else do
    modifiedChild <- (insertEntitySegments oldFromJust rest e)
    return $ Directory p (Map.insert cur (FSDirectory modifiedChild) ch) permissions acc md

insertEntitySegments (Directory p ch permissions acc md) [] e = do
  let old = Map.lookup (displayName e) ch
  if (not (writable permissions) && isNothing old)
  then do
    throwError $ ModifyingNonWritableError p
  else do
    return $ Directory p (Map.insert (displayName e) e ch) permissions acc md

-- |insert entity using its own path
insertEntityNotRetarded :: Directory -> FSEntity -> Either FSError Directory
insertEntityNotRetarded dir e = insertEntity dir (getPath e) e

-- |insert entity by path
insertEntity :: Directory -> FilePath -> FSEntity -> Either FSError Directory
insertEntity fs path e =
  if (equalFilePath path $ dpath fs)
  then do
    case e of
      (FSDirectory dir) -> do
        return dir
      _ -> undefined  --should never happen
  else do
    insertEntitySegments fs (relativePathToParentSplitted fs path) e

-- |remove entity by path
removeEntity :: Directory -> FilePath -> Either FSError Directory
removeEntity fs path = do
  let parentPath = takeDirectory path
  oldParent <- maybeToEither (NoSuchPathError parentPath) $ getDirectory fs parentPath

  boolToEither (ModifyingNonWritableError $ dpath oldParent) $ writable $ dpermissions oldParent
  _ <- maybeToEither (NoSuchPathError path) $ Map.lookup (takeBaseName path <.> takeExtension path) (children oldParent)
  let newParent = oldParent { children = Map.delete (takeBaseName path <.> takeExtension path) (children oldParent) }
  insertEntity fs (dpath newParent) (FSDirectory newParent)

-- |insert new empty dir by path
insertDir :: Directory -> FilePath -> Either FSError Directory
insertDir fs path = do
  let pathTo = relativePathTo fs path
      newDir = FSDirectory $ Directory ((dpath fs) </> pathTo) Map.empty (rwPermissions { executable = True, searchable = True }) Nothing Nothing
  insertEntity fs path newDir

-- |insert file by its content and path
insertFile :: Directory -> FilePath -> ByteString -> Either FSError Directory
insertFile fs path content = do
  let pathTo = relativePathTo fs path
      newFile = FSFile $ File ((dpath fs) </> pathTo) content rwPermissions Nothing Nothing Nothing Nothing
  insertEntity fs path newFile

-- |get splitted relative path to parent of given path
relativePathToParentSplitted :: Directory -> FilePath -> [FilePath]
relativePathToParentSplitted fs path =
  map dropTrailingPathSeparator $ splitPath relativePath
    where
      relativePath = relativePathTo fs $ takeDirectory path

-- |get relative path from given directory to given path
relativePathTo :: Directory -> FilePath -> FilePath
relativePathTo fs path = do
  let relativePath =
        if (isAbsolute path)
        then makeRelative (dpath fs) path
        else path
  if (relativePath == ".")
  then ""
  else relativePath

-- |init vcs in given file
initVcsFile :: File -> Either FSError File
initVcsFile file =
  case (fvcs file) of
    Nothing -> do
      if (readable $ fpermissions file)
      then do
        return $ file { fvcs = Just $ Vcs [Version "initial" (fcontent file)] }
      else
        throwError $ NotReadableError $ fpath file
    _ -> do
      return file

-- |update vcs as current state of file and message
updateVcsFile :: String -> File -> Either FSError File
updateVcsFile msg file =
  case (fvcs file) of
    Nothing -> throwError (NoVcsError $ fpath file)
    Just (Vcs versions) -> return $ file { fvcs = Just $ Vcs $ (Version msg (fcontent file)) : versions }

-- |insert all entities
insertAll :: FSEntityClass a => Directory -> [a] -> Either FSError Directory
insertAll fs rest
    = foldM
        (\fs cur -> insertEntityNotRetarded fs (toFSEntity cur)) fs rest

-- |check permissions, throw error if check failed
checkPermissions :: (Permissions -> Bool) -> FSError -> Permissions -> Either FSError ()
checkPermissions check err permissions = do
  if (check permissions)
  then do
    return ()
  else do
    throwError err

-- |init vcs by path, collect all errors
initVcs :: Directory -> FilePath -> Either FSError (Directory, [FSError])
initVcs fs path = do
  e <- getEntityEither fs path
  let files = listFilesEntities e

  let filesWihVcsAndErrors = partitionEithers $ map initVcsFile files
      filesWihVcs = snd filesWihVcsAndErrors
  newFs <- insertAll fs filesWihVcs
  return (newFs, fst filesWihVcsAndErrors)

-- |update all vcss that can by path
updateVcs :: Directory -> String -> FilePath -> Either FSError Directory
updateVcs fs msg path = do
  e <- getEntityEither fs path
  let files = listFilesEntities e

  filesUpdated <- sequenceA $ filter isRight $ map (updateVcsFile msg) files
  insertAll fs filesUpdated

-- |replace vcs by given
setVcs :: Directory -> Vcs -> FilePath -> Either FSError Directory
setVcs fs vcs path = do
  file <- getFileEither fs path
  let fileWithVcs = file { fvcs = Just vcs }
  insertEntityNotRetarded fs (toFSEntity fileWithVcs)

-- |bool to either
boolToEither :: b -> Bool -> Either b ()
boolToEither err check = do
  if (check)
  then do
    return ()
  else do
    throwError err

-- |maybe to either
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err mb =
  case mb of
    Just a -> Right a
    _      -> Left err

-- |remove vcs from file
removeVcs :: Directory -> FilePath -> Either FSError Directory
removeVcs fs path = do
  file <- getFileEither fs path

  let fileModified = file { fvcs = Nothing }
  insertEntityNotRetarded fs (FSFile fileModified)

-- |remove version from vcs by path and index
removeVersion :: Directory -> FilePath -> Int -> Either FSError Directory
removeVersion fs path index = do
  file <- getFileEither fs path
  vcs <- maybeToEither (NoVcsError path) $ fvcs file

  let versionsReversed = reverse $ vcsversions vcs
  versionRemoved <- deleteAt NoSuchVersionError index versionsReversed
  let fileModified = file { fvcs = Just $ Vcs $ reverse versionRemoved }
  insertEntityNotRetarded fs (FSFile fileModified)

-- |merge two vcs versions by their index and custom merge strategy
mergeVcs :: Directory -> FilePath
         -> (Version -> Version -> Version)  -- merge strategy
         -> Int -> Int  -- ids to merge
         -> Either FSError Directory
mergeVcs fs path mergeStrat id1 id2 = do
  file <- getFileEither fs path
  vcs <- maybeToEither (NoVcsError path) $ fvcs file

  let versionsReversed = reverse $ vcsversions vcs
  version1 <- maybeToEither (NoSuchVersionError id1) $ versionsReversed !!? id1
  version2 <- maybeToEither (NoSuchVersionError id2) $ versionsReversed !!? id2

  let merged = mergeStrat version1 version2
      fileModified = file { fvcs = Just $ Vcs $ merged : vcsversions vcs }
  insertEntityNotRetarded fs (FSFile fileModified)

-- |merge strategies
data MergeStrategy = PureStrat MergeStrategyPure
                   | Interactive
                   deriving (Show)

-- |pure merge strategies
data MergeStrategyPure = TakeLeft
                       | TakeRight
                       | TakeBoth
                       deriving ( Show )

-- |get merge strat by name
getMergeStrat :: MergeStrategyPure -> Version -> Version -> Version
getMergeStrat TakeLeft  = takeLeftVersion
getMergeStrat TakeRight = takeRightVersion
getMergeStrat TakeBoth  = takeBothVersions

-- |default merge message
mergeMessageDefault :: Version -> Version -> String
mergeMessageDefault v1 v2 =
  "Merged revision:" ++ "\n"
  ++"  \"" ++ (addIndentToEveryStringExceptFirst "   " $ vmessage v1) ++ "\"\n"
  ++ "into:" ++ "\n"
  ++ "  \"" ++ (addIndentToEveryStringExceptFirst "   " $ vmessage v2) ++ "\""

-- |left merge strat
takeLeftVersion :: Version -> Version -> Version
takeLeftVersion v1 v2 = Version (mergeMessageDefault v1 v2 ++ "\n" ++ "kept left") $ vcontent v1

-- |right merge strat
takeRightVersion :: Version -> Version -> Version
takeRightVersion v1 v2 = Version (mergeMessageDefault v1 v2 ++ "\n" ++ "kept right") $ vcontent v2

-- |both merge strat
takeBothVersions :: Version -> Version -> Version
takeBothVersions v1 v2 = do
  let diff = getGroupedDiff (B8.toString $ vcontent v1) (B8.toString $ vcontent v2)
      addedArrows =
        map
          (\case
            Both l _ -> l
            First l  -> "<<<" ++ l ++ "<<<"
            Second r -> ">>>" ++ r ++ ">>>"
          )
          diff
  Version (mergeMessageDefault v1 v2 ++ "\n" ++ "kept both") $ B8.fromString $ concat addedArrows

-- |interactive merge strat
interactive :: Version -> Version -> IO Version
interactive v1 v2 = do
  let diff = getGroupedDiff (B8.toString $ vcontent v1) (B8.toString $ vcontent v2)
  addedArrows <-
    mapM
      (\case
        Both l _ -> do
          return l
        First l -> do
          untilJust $ do
            putStrLn $ "<<<\n" ++ l
            putStrLn $ "take: y/n\n";
            yesOrNo <- getLine
            case yesOrNo of
              "y" -> return $ Just l
              "n" -> return $ Just ""
              _   -> return Nothing
        Second r -> do
          untilJust $ do
            putStrLn $ ">>>\n" ++ r
            putStrLn $ "take: y/n\n";
            yesOrNo <- getLine
            case yesOrNo of
              "y" -> return $ Just r
              "n" -> return $ Just ""
              _   -> return Nothing
      )
      diff
  return $ Version (mergeMessageDefault v1 v2 ++ "\n" ++ "merged interacivly") $ B8.fromString $ concat addedArrows
