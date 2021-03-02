{-# LANGUAGE ScopedTypeVariables #-}

module Utils ( dropAllPointPointThatCan
             , replaceEndLines
             , splitOnSpaceButNotSemicolons
             , addIndentToEveryString
             , addIndentToEveryStringExceptFirst
             , (!!?)
             , deleteAt
             ) where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import System.FilePath (joinPath, splitDirectories)

dropTrailingPointPointSegments :: [FilePath] -> [FilePath]
dropTrailingPointPointSegments splitted = dropCounted afterDrop dropped
  where
    dropPointPointAndCount :: [FilePath] -> ([FilePath], Int)
    dropPointPointAndCount (cur : rest)
      | cur == ".." = let (segments, cnt) = dropPointPointAndCount rest in (segments, cnt + 1)
      | otherwise = (cur : rest, 0)
    dropPointPointAndCount [] = ([], 0)

    dropCounted :: [FilePath] -> Int -> [FilePath]
    dropCounted (cur : rest) cnt | cnt > 0 = dropCounted rest (cnt - 1)
                                 | otherwise = cur : rest
    dropCounted [] _ = []

    (afterDrop, dropped) = dropPointPointAndCount splitted

-- |drops all ".." and equal amount of path segments from the path
dropAllPointPointThatCan :: FilePath -> FilePath
dropAllPointPointThatCan path =
  joinPath $ reverse $ dropAllPointPointThatCanSegments splittedReversed
    where
      splitted = splitDirectories path
      splittedReversed = reverse splitted

      dropAllPointPointThatCanSegments :: [FilePath] -> [FilePath]
      dropAllPointPointThatCanSegments (cur : rest)
        | cur == ".." = dropAllPointPointThatCanSegments $ dropTrailingPointPointSegments (cur : rest)
        | otherwise = cur : dropAllPointPointThatCanSegments rest
      dropAllPointPointThatCanSegments [] = []

-- |replace all "\n" to '\n'
replaceEndLines :: String -> String
replaceEndLines s = replaceEndLinesHelper s Nothing
  where
    replaceEndLinesHelper :: String -> Maybe Char -> String
    replaceEndLinesHelper (cur : rest) (Just prev) =
      if (prev == '\\' && cur == 'n')
      then
        '\n' : replaceEndLinesHelper rest Nothing
      else
        prev : replaceEndLinesHelper rest (Just cur)
    replaceEndLinesHelper (cur : rest) Nothing =
      replaceEndLinesHelper rest (Just cur)
    replaceEndLinesHelper "" Nothing = ""
    replaceEndLinesHelper "" (Just lst) = [lst]

-- |split on space, keeping "" substrings not splitted
splitOnSpaceButNotSemicolons :: String -> [String]
splitOnSpaceButNotSemicolons (cur : rest) =
  case cur of
    '\"' -> do
      let
        semicolonsMaybe = do
          untilNextSemicolon <- takeUntilNextSemicolon rest
          return $ [] : untilNextSemicolon : splitOnSpaceButNotSemicolons (drop (length untilNextSemicolon + 1) rest)
      if (isJust semicolonsMaybe)
      then
        fromJust semicolonsMaybe
      else do
        let restProcessed = splitOnSpaceButNotSemicolons rest :: [String]
        (cur : (head restProcessed)) : (tail restProcessed)
    ' ' -> do
      "" : splitOnSpaceButNotSemicolons rest
    _ -> do
      let restProcessed = splitOnSpaceButNotSemicolons rest
      (cur : head restProcessed) : tail restProcessed
    where
      takeUntilNextSemicolon :: String -> Maybe String
      takeUntilNextSemicolon (cur : rest)
        | cur == '"' = return []
        | otherwise  = (cur :) <$> takeUntilNextSemicolon rest
      takeUntilNextSemicolon [] = Nothing

splitOnSpaceButNotSemicolons [] = [""]

-- |add indent to all lines except first
addIndentToEveryStringExceptFirst :: String -> String -> String
addIndentToEveryStringExceptFirst indent str = do
  let splitted = lines str
  intercalate "\n" $ head splitted : (map (indent ++) $ tail splitted)

-- |add indent to every line
addIndentToEveryString :: String -> String -> String
addIndentToEveryString indent str = indent ++ addIndentToEveryStringExceptFirst indent str

-- |safe take n-th element
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0     = Nothing
  | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x : _)  = Just x
    go j (_ : ys) = go (j - 1) ys
    go _ []       = Nothing

-- |safe deleteAt
deleteAt :: forall a b.(Int -> b) -> Int -> [a] -> Either b [a]
deleteAt err i ls
  | i < 0 = throwError $ err i
  | otherwise = go i ls
  where
    go :: Int -> [a] -> Either b [a]
    go 0 (_ : xs) = return xs
    go n (x : xs) = do
      goxs <- go (n - 1) xs
      return $ x : goxs
    go 0 []       = return []
    go _ []       = throwError $ err i
