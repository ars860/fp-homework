{-# LANGUAGE TemplateHaskell  #-}

module Task5 ( FS (..), name, contents, _File, _Dir, copyFs ) where

import Lens.Micro ( Lens', Traversal', lens )
import System.Directory as D ( Permissions (..)
                             , canonicalizePath
                             , doesDirectoryExist
                             , listDirectory
                             , getPermissions
                             )
import System.FilePath ( takeBaseName
                       , takeFileName
                       , (</>)
                       )

data FS
    = Dir
    { _name     :: FilePath  -- название папки, не полный путь
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath  -- название файла, не полный путь
    }
    deriving ( Show, Eq )

-- |_name lens
name :: Lens' FS FilePath
name = lens _name (\fs nm -> fs { _name = nm })

-- |_contents traversal
contents :: Traversal' FS [FS]
contents = _Dir . lens _contents (\fs cnt -> fs { _contents = cnt })

-- |Dir prism
_Dir :: Traversal' FS FS
_Dir f (Dir n c) = f (Dir n c)
_Dir _ (File n) = pure (File n)

-- |File prism
_File :: Traversal' FS FS
_File _ (Dir n c) = pure (Dir n c)
_File f (File n) = f (File n)

-- |copy fs from given path
copyFs :: FilePath -> IO FS
copyFs path = do
  isDir <- doesDirectoryExist path
  pathCanonicalized <- canonicalizePath path
  permissions <- D.getPermissions path

  if isDir
  then do
    if (readable permissions && searchable permissions)
    then do
      pathChildren <- listDirectory path
      childrenFs <- mapM (copyFs . (path </>)) pathChildren
      return $ Dir (takeBaseName pathCanonicalized) childrenFs
    else do
      return $ Dir (takeBaseName pathCanonicalized) []
  else do
    return $ File (takeFileName pathCanonicalized)
