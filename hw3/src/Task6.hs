{-# LANGUAGE RankNTypes #-}

module Task6 ( cd, ls, file ) where

import Lens.Micro ( Traversal', traversed, filtered )
import System.FilePath ()

import Task5 ( FS (..), name, contents, _File, _Dir )

-- |cd
cd :: FilePath -> Traversal' FS FS
cd nm = contents . traversed . _Dir . filtered ((== nm) . _name)

-- |ls
ls :: Traversal' FS FilePath
ls = contents . traversed . name

-- |find file by name
file :: FilePath -> Traversal' FS String
file nm = contents . traversed . _File . name . filtered (== nm)
