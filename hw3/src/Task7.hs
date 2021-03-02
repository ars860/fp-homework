{-# LANGUAGE RankNTypes #-}

module Task7 ( changeExtensions, allNames, deleteEmpty, move, getPath ) where

import System.FilePath ( (-<.>), (</>), takeFileName )
import Lens.Micro ( Traversal', traversed, (^.)
                  , (.~), (&), (%~)
                  , each, filtered, lens
                  , (^..), to, SimpleFold
                  )

import Task5 ( FS (..), name, contents, _File, _Dir )

-- |Changes all extensions with given one
changeExtensions :: String -> FS -> FS
changeExtensions ext fs =  fs & contents . traversed . _File . name %~ (-<.> ext)

-- |Shows all names recursively
allNames :: FS -> [FilePath]
allNames fs = (_name fs)
            : concat ((fs ^.. contents . traversed . _File . name)
            : map allNames (fs ^.. contents . traversed . _Dir))

-- |IsDir
isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _ = False

-- |Removes given dir by name if it is
deleteEmpty :: FS -> FilePath -> FS
deleteEmpty fs nm = fs & contents .~ fs  ^.. contents . traversed . filtered (\e -> not (isDir e && _name e == nm && null (_contents e)))

-- |Moves to given child by its name
move :: FilePath -> SimpleFold FS FS
move nm =
  to (\root -> root & contents . each . filtered ((==nm) . (^. name)) %~ (\prev -> prev & name .~ root ^. name </> prev ^. name))
  . contents . each . filtered ((==nm) . takeFileName . (^. name))

-- |Gets current path relative to the start of moving
getPath :: Traversal' FS FilePath
getPath = name
