{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( fun1
  , fun2
  , fun3
  ) where

import Data.Either

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _  = False

-- | function that returns False, but hard way
fun1 :: Bool
fun1 =
  ((
    (($) :: ([String] -> Bool) -> [String] -> Bool)
    ((
      ((
        ((.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool)
        (Task7.null :: String -> Bool)
      ) :: ([String] -> String) -> [String] -> Bool)
      (head :: [String] -> String)
    ) :: [String] -> Bool)
  ) :: [String] -> Bool)
  ((
    ((
      (map :: ((String -> String, String) -> String) ->
              [(String -> String, String)] -> [String])
      ((
        (uncurry :: ((String -> String) -> String -> String) ->
                    (String -> String, String) -> String)
        (id :: (String -> String) -> String -> String)
      ) :: (String -> String, String) -> String)
    ) :: [(String -> String, String)] -> [String])
    ([ (((++) :: String -> String -> String) ("Dorian " :: String)
     , (" Grey" :: String))
    ] :: [(String -> String, String)])
  ) :: [String])

-- | function that takes two numbers and returns list with pair of them
fun2 :: forall a b. (Num a, Num b) => [(b, a)]
fun2 =
  ((
    \(x :: [Either d c]) ->
    ((
      ((
        (zip :: [d] -> [c] -> [(d, c)])
        ((
          (lefts :: [Either d c] -> [d])
          (x :: [Either d c])
        ) :: [d])
      ) :: [c] -> [(d, c)])
      ((
        (rights :: [Either d c] -> [c])
        (x :: [Either d c])
      ) :: [c])
    ) :: [(d, c)])
  ) :: forall c d. [Either d c] -> [(d, c)])
  ([ ((
       Left
         ((
           ((
             ((+) :: b -> b -> b)
             (1 :: b)
           ) :: b -> b)
           (2 :: b)
         ) :: b)
     ) :: Either b a)
   , ((
       Right
         ((
           ((
             ((^) :: Integral c => a -> c -> a)
             (2 :: a)
           ) :: Integral c => c -> a)
           (6 :: Integer)
         ) :: a)
     ) :: Either b a)
  ] :: [Either b a])

-- | function that takes integer number a
-- and returns (a % 4 == 0) -> (a % 2 == 0)
-- where (->) is boolean implication
fun3 :: forall a. Integral a => a -> Bool
fun3 =
  let impl =
        ((
          \(x :: Bool) (y :: Bool) ->
          ((
            (not :: Bool -> Bool)
              ((
                ((
                  ((||) :: Bool -> Bool -> Bool)
                  (x :: Bool)
                ) :: Bool -> Bool)
                (y:: Bool)
              ) :: Bool)
          ) :: Bool)
        ) :: Bool -> Bool -> Bool)
  in
    let isMod2 =
          ((
            \(x :: a) ->
            ((
              ((==) :: a -> a -> Bool)
              ((
                ((
                  (mod :: a -> a -> a)
                  (x :: a)
                ) :: a -> a)
                (2 :: a)
              ) :: a)
              (0 :: a)
            ) :: Bool)
          ) :: a -> Bool)
    in
      let isMod4 =
            ((
              \(x :: a) ->
              ((
                ((
                  ((==) :: a -> a -> Bool)
                  ((
                    ((
                      (mod :: a -> a -> a)
                      (x :: a)
                    ) :: a -> a)
                    (4 :: a)
                  ) :: a)
                ) :: a -> Bool)
                (0 :: a)
              ) :: Bool)
            ) :: a -> Bool)
      in
        ((
          \(x :: a) ->
          ((
            ((
              (impl :: Bool -> Bool -> Bool)
              ((
                (isMod4 :: a -> Bool)
                (x :: a)
              ) :: Bool)
            ) :: Bool -> Bool)
            ((
              (isMod2 :: a -> Bool)
              (x :: a)
            ) :: Bool)
          ) :: Bool)
        ) :: a -> Bool)
