{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException (..), catch)
import Control.Monad.Except (ExceptT (..), lift, liftIO, runExceptT, throwError, unless, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.State (StateT (..), get, put)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (append)
import qualified Data.ByteString.Char8 as BIO (putStrLn)
import Data.Char (isDigit, isLetter)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative (Parser (..), ParserFailure (..), ParserInfo (..), ParserResult (..),
                            argument, auto, command, defaultPrefs, eitherReader, execParser,
                            execParserPure, fullDesc, header, help, helper, info, maybeReader,
                            metavar, progDesc, str, subparser, (<**>))
import System.Directory (Permissions (..), doesDirectoryExist)
import System.Environment (getExecutablePath, getProgName)
import System.FilePath (dropTrailingPathSeparator, equalFilePath, isAbsolute, makeRelative,
                        normalise, splitPath, takeBaseName, takeDirectory, takeExtension,
                        takeFileName, (<.>), (</>))
import System.IO (hFlush, stdout)

import FileSystem as FS
import Utils

-- |possible command to operate fs
data Command = Dir
             | Ls FilePath
             | Cd FilePath
             | Exit
             | MkDir String
             | Touch String
             | Tree
             | Chill
             | Cat FilePath
             | Rm FilePath
             | Append FilePath String
             | Write FilePath String
             | Find String
             | Info FilePath
             | VcsInit FilePath
             | VcsUpdate FilePath String
             | VcsHistory FilePath
             | VcsCat FilePath Int
             | VcsRemoveVersion FilePath Int
             | VcsRemove FilePath
             | VcsMerge FilePath Int Int MergeStrategy
             | VcsSet FilePath Int
             deriving ( Show )

-- |command parser
dir :: Parser Command
dir = pure Dir

-- |command parser
exit :: Parser Command
exit = pure Exit

-- |command parser
tree :: Parser Command
tree = pure Tree

-- |command parser
ls :: Parser Command
ls = Ls <$> argument str (metavar "PATH")

-- |command parser
cd :: Parser Command
cd = Cd <$> argument str (metavar "PATH")

-- |command parser
cat :: Parser Command
cat = Cat <$> argument str (metavar "PATH")

-- |command parser
rm :: Parser Command
rm = Rm <$> argument str (metavar "PATH")

-- |command parser
append :: Parser Command
append = Append <$> argument str (metavar "PATH") <*> (argument str (metavar "CONTENT"))

-- |command parser
write :: Parser Command
write = Write <$> argument str (metavar "PATH") <*> (argument str (metavar "CONTENT"))

-- |validate dir name to contain only letters/digits/_
validateDirName :: String -> Maybe String
validateDirName str =
  if str == filter (\s -> isLetter s || isDigit s || s == '_') str
  then Just str
  else Nothing

-- |command parser
mkdir :: Parser Command
mkdir = MkDir <$> argument (maybeReader validateDirName) (metavar "DIR_NAME" <> help "name can be only letters or digits or _")

-- |validate file name to contain only letters/digits/_/.
validateFileName :: String -> Maybe String
validateFileName str =
  if str == filter (\s -> isLetter s || isDigit s || s == '_' || s == '.') str && str /= vcsFileName
  then Just str
  else Nothing

-- |command parser
touch :: Parser Command
touch = Touch <$> argument (maybeReader validateFileName) (metavar "FILE_NAME" <> help "name can be only letters or digits or _")

-- |command parser
findParser :: Parser Command
findParser = Find <$> argument (maybeReader validateFileName) (metavar "FILE_NAME" <> help "name can be only letters or digits or _")

-- |command parser
infoParser :: Parser Command
infoParser = Info <$> argument str (metavar "PATH")

-- |command parser
vcsInit :: Parser Command
vcsInit = VcsInit <$> argument str (metavar "PATH")

-- |command parser
vcsUpdate :: Parser Command
vcsUpdate = VcsUpdate <$> argument str (metavar "PATH") <*> (argument str (metavar "MSG"))

-- |command parser
vcsHistory :: Parser Command
vcsHistory = VcsHistory <$> argument str (metavar "PATH")

-- |command parser
vcsCat :: Parser Command
vcsCat = VcsCat <$> argument str (metavar "PATH") <*> argument auto (metavar "INDEX")

-- |command parser
vcsSet :: Parser Command
vcsSet = VcsSet <$> argument str (metavar "PATH") <*> argument auto (metavar "INDEX")

-- |command parser
vcsRemoveVersion :: Parser Command
vcsRemoveVersion = VcsRemoveVersion <$> argument str (metavar "PATH") <*> argument auto (metavar "INDEX")

-- |command parser
vcsRemove :: Parser Command
vcsRemove = VcsRemove <$> argument str (metavar "PATH")

-- |parse merge strategy from string
parseMergeStrat :: String -> Either String MergeStrategy
parseMergeStrat str =
  case str of
    "left" -> return $ PureStrat TakeLeft
    "right" -> return $ PureStrat TakeRight
    "both" -> return $ PureStrat TakeBoth
    "interactive" -> return Interactive
    _ -> throwError $ "Unknown merge strategy: \"" ++ str ++ "\". Avaliable strategies: right | left | both | interactive"

-- |command parser
vcsMerge :: Parser Command
vcsMerge =
  VcsMerge <$> argument str (metavar "PATH")
           <*> argument auto (metavar "INDEX_1")
           <*> argument auto (metavar "INDEX_2")
           <*> argument (eitherReader parseMergeStrat) (metavar "MERGE_STRATEGY")

-- |parser for all commands
commandParser :: Parser Command
commandParser = subparser
  (  command "dir"         (info dir (progDesc "show current dir chidren"))
  <> command "ls"          (info ls (progDesc "show seleceted dir children"))
  <> command "cd"          (info cd (progDesc "change current directory"))
  <> command "exit"        (info exit (progDesc "exit"))
  <> command "mkdir"       (info mkdir (progDesc "make dir"))
  <> command "tree"        (info tree (progDesc "draws current dir tree"))
  <> command "touch"       (info touch (progDesc "create empty file"))
  <> command "cat"         (info cat (progDesc "show file content"))
  <> command "rm"          (info rm (progDesc "remove file/dir"))
  <> command "append"      (info append (progDesc "append to file"))
  <> command "write"       (info write (progDesc "write to file (overwrite)"))
  <> command "find"        (info findParser (progDesc "find file by name in current dir"))
  <> command "info"        (info Main.infoParser (progDesc "info by path"))
  <> command "vcs-init"    (info vcsInit (progDesc "init vcs in given directory/file"))
  <> command "vcs-update"  (info vcsUpdate (progDesc "add this file/directory vcs"))
  <> command "vcs-history" (info vcsHistory (progDesc "show file history"))
  <> command "vcs-cat"     (info vcsCat (progDesc "show selected file version"))
  <> command "vcs-rmver"   (info vcsRemoveVersion (progDesc "remove version by id"))
  <> command "vcs-remove"  (info vcsRemove (progDesc "remove file from vcs"))
  <> command "vcs-merge"   (info vcsMerge (progDesc "merge two revisions"))
  <> command "vcs-set"     (info vcsSet (progDesc "set file content to selected revision"))
  )

-- |parser for dir in which to init fs
initialDirParser :: Parser FilePath
initialDirParser = argument str (metavar "DIR")

-- |environment to store fs and some changable fields
data Environment = Environment { prefix        :: String
                               , currentPath   :: FilePath
                               , programStatus :: ProgramStatus
                               , parser        :: ParserInfo Command
                               , fileSeparator :: String
                               , fs            :: Directory
                               , rms           :: [FilePath]
                               }

-- |program status
data ProgramStatus = Running
                   | Stopped
                   deriving (Show, Eq)

-- |main
main :: IO ()
main = do
  setLocaleEncoding utf8
  initialDirRelative <- execParser $ info initialDirParser mempty
  executablePath <- getExecutablePath
  let initialDir = dropAllPointPointThatCan $ dropTrailingPathSeparator $ normalise $ (takeDirectory executablePath) </> initialDirRelative
  isDir <- doesDirectoryExist initialDir
  if (not isDir)
  then do
    putStrLn $ "Initial dir: \"" ++ initialDir ++ "\" is not a directory"
  else do
    (FSDirectory fsCopy) <- copyFs initialDir
    (fsWithVcs, errors) <- loadVcs fsCopy `catch` (\(e :: SomeException) -> putStrLn "WARN: can't load vcs" >> return (fsCopy, []))
    unless (null errors) $ do
      putStrLn $ "WARN: Some errors during vcs import\n(Save .vcs file manually to prevent losing it)"
      mapM_ print errors
    let opts =
          info (commandParser <**> helper)
            (  fullDesc
            <> progDesc "Best filesystem manager in the entire universe"
            <> header "Filozavr"
            )
    (_, env) <- flip runStateT (Environment "> " (normalise initialDir) Running opts "   " fsWithVcs []) $ whileM_ checkStatus $ execCommand
    syncWithFs (fs env) (rms env) `catch` (\(e :: SomeException) -> putStrLn $ "Panic! Can't sync! " ++ (show e))

-- |check if status is running
checkStatus :: StateT Environment IO Bool
checkStatus = do
  env <- get
  return $ programStatus env == Running

-- |exec command
execCommand :: StateT Environment IO ()
execCommand = do
  env <- get
  liftIO $ putStr ((currentPath env) ++ (prefix env))
  liftIO $ hFlush stdout
  input <- liftIO $ getLine
  let splittedInput = filter (not . null) $ splitOnSpaceButNotSemicolons input
  let currentCommand =
        if (null input)
        then (Success Chill)
        else execParserPure defaultPrefs (parser env) splittedInput
  case currentCommand of
    (Failure (ParserFailure help)) -> do
      progName <- liftIO $ getProgName
      let (helpMsg, _, _) = help progName
      liftIO $ print helpMsg
    (Success c) -> do
      case c of
        Exit -> do
          let newEnv = env { programStatus = Stopped }
          put newEnv
        Dir -> do
          execLs (currentPath env)
        Ls dir -> do
          execLs dir
        Cd newPath -> do
          execCd newPath
        MkDir name -> do
          execMkDir name
        Tree -> do
          execTree
        Chill -> do
          return ()
        Touch name -> do
          execTouch name
        Cat path -> do
          execCat path
        Rm path -> do
          execRm path
        Append path content -> do
          execAppend path content
        Write path content -> do
          execWrite path content
        Find name -> do
          execFind name
        Info path -> do
          execInfo path
        VcsInit path -> do
          execVcsInit path
        VcsUpdate path msg -> do
          execVcsUpdate path msg
        VcsHistory path -> do
          execVcsHistory path
        VcsCat path id -> do
          execVcsCat path id
        VcsRemoveVersion path id -> do
          execRemoveVersion path id
        VcsRemove path -> do
          execVcsRemove path
        VcsMerge path index index2 strat -> do
          execVcsMerge path index index2 strat
        VcsSet path index -> do
          execVcsSet path index

-- a lot of adaptation of fs function to better work on environment

-- |get directory using current path and fs from environment
getDirectoryEnv :: FilePath -> StateT Environment IO (Maybe Directory)
getDirectoryEnv path = do
  env <- get
  absolute <- toAbsolutePath path
  return $ getDirectory (fs env) absolute

-- |get file using current path and fs from environment
getFileEnv :: FilePath -> StateT Environment IO (Maybe File)
getFileEnv path = do
  env <- get
  absolute <- toAbsolutePath path
  return $ getFile (fs env) absolute

-- |get entity using current path and fs from environment
getEntityEnv :: FilePath -> StateT Environment IO (Maybe FSEntity)
getEntityEnv path = do
  env <- get
  absolute <- toAbsolutePath path
  return $ getEntity (fs env) absolute

-- |get directory using current path and fs from environment.
-- Call only if given path is a directory for sure
getDirectoryEnvUnsafe :: FilePath -> StateT Environment IO Directory
getDirectoryEnvUnsafe path = do
  absolute <- toAbsolutePath path
  maybeDir <- getDirectoryEnv absolute
  return $ fromJust maybeDir

-- |is directory using current path and fs from environment
isDirectoryEnv :: FilePath -> StateT Environment IO Bool
isDirectoryEnv path = do
  env <- get
  absolute <- toAbsolutePath path
  return $ isDirectory (fs env) absolute

-- |is file using current path and fs from environment
isFileEnv :: FilePath -> StateT Environment IO Bool
isFileEnv path = do
  maybeFile <- getFileEnv path
  return $ isJust maybeFile

-- |list directory using current path and fs from environment
listDirectoryEnv :: FilePath -> StateT Environment IO [FilePath]
listDirectoryEnv path = do
  env <- get
  return $ listDirPath (fs env) path

-- |insert empty dir env
insertEmptyDirEnv :: FilePath -> ExceptT FSError (StateT Environment IO) ()
insertEmptyDirEnv path = do
  env <- get
  absolute <- lift $ toAbsolutePath path
  newFs <- eitherToExcept $ insertDir (fs env) absolute
  lift $ put $ env { fs = newFs }

-- |insert entity env
insertEntityEnv :: FSEntity -> ExceptT FSError (StateT Environment IO) ()
insertEntityEnv e = do
  env <- get
  newFs <- eitherToExcept $ insertEntity (fs env) (getPath e) e
  lift $ put $ env { fs = newFs }

-- |insert file by content env
insertFileByContentEnv :: FilePath -> ByteString -> ExceptT FSError (StateT Environment IO) ()
insertFileByContentEnv path content = do
  env <- get
  newFs <- eitherToExcept $ insertFile (fs env) path content
  lift $ put $ env { fs = newFs }

-- |remove entity env
removeEntityEnv :: FilePath -> ExceptT FSError (StateT Environment IO) ()
removeEntityEnv path = do
  env <- get
  absolute <- lift $ toAbsolutePath path
  newFs <- eitherToExcept $ removeEntity (fs env) absolute
  lift $ put $ env { fs = newFs, rms =  absolute : rms env }

-- |init vcs env
initVcsEnv :: FilePath -> ExceptT FSError (StateT Environment IO) [FSError]
initVcsEnv path = do
  env <- get
  absolute <- lift $ toAbsolutePath path
  (newFs, errors) <- eitherToExcept $ initVcs (fs env) absolute
  lift $ put $ env { fs = newFs }
  return errors

-- |update vcs env
updateVcsEnv :: FilePath -> String -> ExceptT FSError (StateT Environment IO) ()
updateVcsEnv path msg = do
  env <- get
  newFs <- eitherToExcept $ updateVcs (fs env) msg (normalise $ (currentPath env) </> path)
  lift $ put $ env { fs = newFs }

-- |remove version env
removeVersionEnv :: FilePath -> Int -> ExceptT FSError (StateT Environment IO) ()
removeVersionEnv path index = do
  env <- get
  newFs <- eitherToExcept $ removeVersion (fs env) (normalise $ (currentPath env) </> path) index
  lift $ put $ env { fs = newFs }

-- |remove vcs env
vcsRemoveEnv :: FilePath -> ExceptT FSError (StateT Environment IO) ()
vcsRemoveEnv path = do
  env <- get
  newFs <- eitherToExcept $ removeVcs (fs env) (normalise $ (currentPath env) </> path)
  lift $ put $ env { fs = newFs }

-- |convert given path to absolute using current path
toAbsolutePath :: FilePath -> StateT Environment IO FilePath
toAbsolutePath path = do
  env <- get
  return $ dropAllPointPointThatCan $ normalise $ (currentPath env) </> path

-- |perform cd command
execCd :: FilePath -> StateT Environment IO ()
execCd path = execExcept $ do
  env <- lift $ get
  absolute <- lift $ toAbsolutePath path
  newCurrentDir <- maybeToExceptM (NotDirectoryError absolute) $ getDirectoryEnv path
  eitherToExcept $ checkPermissions searchable (NotSearchableError absolute) $ dpermissions newCurrentDir
  put $ env { currentPath = absolute }

-- |perform ls command
execLs :: FilePath -> StateT Environment IO ()
execLs path = execExcept $ do
  env <- lift $ get
  absolute <- lift $ toAbsolutePath path
  directory <- maybeToExceptM (NotDirectoryError absolute) $ getDirectoryEnv path
  eitherToExcept $ checkPermissions readable (NotReadableError absolute) $ dpermissions directory
  entries <- lift $ listDirectoryEnv absolute
  liftIO $ do
    mapM_ (putStr . (++ (fileSeparator env))) entries
    putStrLn ""

-- |perform mkDir command
execMkDir :: String -> StateT Environment IO ()
execMkDir newDirName = execExcept $ do
  absolute <- lift $ toAbsolutePath newDirName
  newDirExists <- boolToExceptM (DirectoryAlreadyExistError absolute) $ isDirectoryEnv newDirName >>= return . not
  insertEmptyDirEnv absolute

-- |perform tree command
execTree :: StateT Environment IO ()
execTree = do
  env <- get
  currentDir <- getDirectoryEnvUnsafe "."
  liftIO $ putStrLn $ treePrint 0 currentDir

-- |perform touch command
execTouch :: FilePath -> StateT Environment IO ()
execTouch newFileName = execExcept $ do
  env <- get
  let newFilePath = normalise $ (currentPath env) </> newFileName
  boolToExceptM (FileAlreadyExistError newFilePath) $ (isFileEnv newFilePath >>= \b -> return $ not b)
  insertFileByContentEnv newFilePath (fromString "")

-- |perform cat command
execCat :: FilePath -> StateT Environment IO ()
execCat path = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path

  _ <- eitherToExcept $ checkPermissions readable (NotReadableError absolute) $ fpermissions file
  liftIO $ BIO.putStrLn $ fcontent file

-- |perform rm command
execRm :: FilePath -> StateT Environment IO ()
execRm path = execExcept $ do
  removeEntityEnv path

-- |perform append command
-- (Append text to file)
execAppend :: FilePath -> String -> StateT Environment IO ()
execAppend path content = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  _ <- eitherToExcept $ checkPermissions writable (NotWritableError absolute) $ fpermissions file

  let newFile = file { fcontent = B.append (fcontent file) (fromString $ replaceEndLines content) }
  insertEntityEnv (FSFile newFile)

-- |perform write command
-- (write ext to file (overwrite))
execWrite :: FilePath -> String -> StateT Environment IO ()
execWrite path content = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  _ <- eitherToExcept $ checkPermissions writable (NotWritableError absolute) $ fpermissions file

  let newFile = file { fcontent = fromString $ replaceEndLines content }
  insertEntityEnv (FSFile newFile)

-- |perform find command
execFind :: String -> StateT Environment IO ()
execFind name = do
  env <- get
  currentDir <- getDirectoryEnvUnsafe "."

  let entries = listDirFiles (FSDirectory currentDir)
      filteredEntries = filter (isInfixOf name . fst) entries
  if (null filteredEntries)
  then do
    liftIO $ putStrLn $ "No files containing this: " ++ name ++ " as substring"
  else do
    liftIO $ mapM_ (\(name, path) -> putStrLn $ name ++ " : " ++ path) filteredEntries

-- |perform info command
execInfo :: FilePath -> StateT Environment IO ()
execInfo path = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  e <- maybeToExceptM (NoSuchPathError absolute) $ getEntityEnv path
  let nameAndPath = (displayName e) ++ " : " ++ (getPath e)
      permissionsAndTimes =
        "Permissions: " ++ (if (readable (FS.getPermissions e)) then "r" else "-")
                        ++ (if (writable (FS.getPermissions e)) then "w" else "-")
                        ++ (if (executable (FS.getPermissions e)) then "x" else "-")
                        ++ (if (searchable (FS.getPermissions e)) then "s" else "-") ++ "\n" ++  -- searchable == can cd dir
        "Last accessed: " ++ (if (isJust $ getAccess e) then show $ fromJust $ getAccess e else "directory created during this run") ++ "\n" ++
        "Last modified: " ++ (if (isJust $ getModification e) then show $ fromJust $ getModification e else "directory created during this run")
      sizeInBytes = "Size: " ++ (show $ getSize e) ++ " bytes"
  liftIO $ putStrLn $ case e of
    (FSDirectory dir) ->
      "Directory: " ++ nameAndPath ++ "\n" ++
      "Total files: " ++ (show $ length $ listDirFiles (FSDirectory dir)) ++ "\n" ++
      sizeInBytes ++ "\n" ++
      permissionsAndTimes
    (FSFile file) ->
      "File: " ++ nameAndPath ++ "\n" ++
      sizeInBytes ++ "\n" ++
      permissionsAndTimes

-- |perform vcs-init command
execVcsInit :: FilePath -> StateT Environment IO ()
execVcsInit path = execExcept $ do
  errors <- initVcsEnv path
  liftIO $ mapM_ (putStrLn . ("Can't init vcs: " ++) . show) errors

-- |perform vcs-update command
execVcsUpdate :: FilePath -> String -> StateT Environment IO ()
execVcsUpdate path msg = execExcept $ do
  updateVcsEnv path msg

-- |perform vcs-history command
execVcsHistory :: FilePath -> StateT Environment IO ()
execVcsHistory path = execExcept $ do
  env <- lift $ get
  absolute <- lift $ toAbsolutePath path
  entity <- maybeToExceptM (NoSuchPathError absolute) $ getEntityEnv path
  let files = listDirFiles entity
  mapM_
    (\(n, p) -> do
      historyEither <- lift $ runExceptT $ execVcsHistoryFile p
      case historyEither of
        Right history -> do
          liftIO $ putStrLn $ n ++ ":"
          let historyIndented = addIndentToEveryString "  " $ intercalate "\n" history
          liftIO $ putStrLn historyIndented
        Left _ -> do
          return ()
    )
    files

-- |vcs history for one file
execVcsHistoryFile :: FilePath -> ExceptT FSError (StateT Environment IO) [String]
execVcsHistoryFile path = do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  vcs <- maybeToExcept (NoVcsError absolute) $ fvcs file
  let indexIndentLength index = (length $ show index) + 2
      indexIndent index = replicate (indexIndentLength index) ' '
      messageFormatted index version = drop (indexIndentLength index) $ intercalate "\n" $ map ((indexIndent index) ++) $ lines $ vmessage version
  return $ map
    (\(index, version) -> (show index) ++": " ++ (messageFormatted index version))
    (zip [0..] $ reverse $ vcsversions vcs)

-- |run except and print error
execExcept :: ExceptT FSError (StateT Environment IO) () -> StateT Environment IO ()
execExcept action = do
  eitherError <- runExceptT action

  case eitherError of
    Left err -> do
      liftIO $ print err
    _ -> do
      return ()

-- |maybeM to exceptT
-- Just a -> return a
-- Nothing -> throwError "given error"
maybeToExceptM :: b -> StateT Environment IO (Maybe a) -> ExceptT b (StateT Environment IO) a
maybeToExceptM err actionMaybe = do
  mb <- lift $ actionMaybe

  case mb of
    Nothing -> do
      throwError err
    Just res -> do
      return res

-- |maybe to exceptT, same as maybeToExceptM but not monadic
maybeToExcept :: b -> Maybe a -> ExceptT b (StateT Environment IO) a
maybeToExcept err mb = do
  case mb of
    Nothing -> do
      throwError err
    Just res -> do
      return res

-- |convert either to exceptT
eitherToExcept :: Either a b -> ExceptT a (StateT Environment IO) b
eitherToExcept either = do
  case either of
    Left err -> do
      throwError err
    Right res -> do
      return res

-- |bool to exceptT, by meaning same as maybeToExcept
boolToExceptM :: b -> StateT Environment IO Bool -> ExceptT b (StateT Environment IO) ()
boolToExceptM err actionBool = do
  check <- lift $ actionBool

  if (check)
  then do
    return ()
  else do
    throwError err

-- |perform vcs-cat command
execVcsCat :: FilePath -> Int -> StateT Environment IO ()
execVcsCat path index = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  vcs <- maybeToExcept (NoVcsError absolute) $ fvcs file
  version <- maybeToExcept (NoSuchVersionError index) $ (reverse $ vcsversions vcs) !!? index
  liftIO $ BIO.putStrLn $ vcontent version

-- |perform vcs-rmver command
execRemoveVersion :: FilePath -> Int -> StateT Environment IO ()
execRemoveVersion path index = execExcept $ do
  removeVersionEnv path index

-- |perform vcs-remove command
execVcsRemove :: FilePath -> StateT Environment IO ()
execVcsRemove path = execExcept $ do
  vcsRemoveEnv path

-- |perform vcs-merge command
execVcsMerge :: FilePath -> Int -> Int -> MergeStrategy -> StateT Environment IO ()
execVcsMerge path id1 id2 strat = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  vcs <- maybeToExcept (NoVcsError absolute) $ fvcs file
  version1 <- maybeToExcept (NoSuchVersionError id1) $ (reverse $ vcsversions vcs) !!? id1
  version2 <- maybeToExcept (NoSuchVersionError id2) $ (reverse $ vcsversions vcs) !!? id2
  case strat of
    PureStrat mergeStratPure -> do
      let mergeStrat = getMergeStrat mergeStratPure
          merged = mergeStrat version1 version2
          fileModified = file { fvcs = Just $ Vcs $ merged : (vcsversions $ fromJust $ fvcs file) }
      insertEntityEnv $ FSFile fileModified
    Interactive -> do
      merged <- liftIO $ interactive version1 version2
      let fileModified = file { fvcs = Just $ Vcs $ merged : (vcsversions $ fromJust $ fvcs file) }
      insertEntityEnv $ FSFile fileModified

-- |perform vcs-set command
-- (set current file contetnt to selected revision)
execVcsSet :: FilePath -> Int -> StateT Environment IO ()
execVcsSet path index = execExcept $ do
  absolute <- lift $ toAbsolutePath path
  file <- maybeToExceptM (NotFileError absolute) $ getFileEnv path
  vcs <- maybeToExcept (NoVcsError absolute) $ fvcs file
  version <- maybeToExcept (NoSuchVersionError index) $ (reverse $ vcsversions vcs) !!? index
  let fileModified = file { fcontent = vcontent version }
  insertEntityEnv $ FSFile fileModified
