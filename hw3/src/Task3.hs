{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleInstances   #-}

module Task3 ( HalyavaScriptExpr (..), ArithExpr (..), Variable (..)
             , VarHolder (..), Something (..), emptyHolder, interpretHalyavaScript
             , callHalyavaFunction1, callHalyavaFunction2, (@=), (#)
             , fromSomething, getVariable, HalyavaShow (..), halyavaOddPow
             , interpretAndUnwrap
             ) where

import Data.Map ( Map )
import Data.Maybe ( isJust, fromJust )
import Data.Typeable ( Typeable, eqT, (:~:) (..))
import qualified Data.Map as Map ( insert, empty, lookup)
import Control.Monad.State.Lazy ( State, get, put, evalState)

-- |Show for halyava script
class HalyavaShow a where
  hShow :: a -> String

instance HalyavaShow Int where
  hShow = show

instance HalyavaShow String where
  hShow = show

instance HalyavaShow Bool where
  hShow b = if (b) then "true" else "false"

-- |Data type representing expression with supported types
data ArithExpr a where
  AENum      :: Int -> ArithExpr Int
  AEBool     :: Bool -> ArithExpr Bool
  AENot      :: ArithExpr Bool -> ArithExpr Bool
  AEString   :: String -> ArithExpr String
  AEPlus     :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEMul      :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEDiv      :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEMod      :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AESub      :: ArithExpr Int -> ArithExpr Int -> ArithExpr Int
  AEAnd      :: ArithExpr Bool -> ArithExpr Bool -> ArithExpr Bool
  AEGt       :: ArithExpr Int -> ArithExpr Int -> ArithExpr Bool
  AEEq       :: (Show a, Eq a) => ArithExpr a -> ArithExpr a -> ArithExpr Bool
  AEConcat   :: ArithExpr String -> ArithExpr String -> ArithExpr String
  AEVar      :: Typeable b => Variable b -> ArithExpr b
  ToString   :: (Show a) => ArithExpr a -> ArithExpr String

-- |Data for accessing variables values during execution
-- Kinda key to access value
data Variable a = Variable { vindex :: Int } deriving ( Eq, Ord, Show )

-- |Existential data for storing variable value
data SomeVariable where
  SomeVariable :: (Show a, Typeable a) => a -> SomeVariable

instance Show SomeVariable where
  show (SomeVariable a) = show a

-- |Variables value storage
data VarHolder = VarHolder { values :: Map Int SomeVariable
                           , lastIndex :: Int
                           } deriving ( Show )

-- |Empty storage
emptyHolder :: VarHolder
emptyHolder = VarHolder Map.empty 0

-- |Get variable value by variable
getVariable :: (Show a, Typeable a) => VarHolder -> Variable a -> Maybe a
getVariable holder ((Variable i) :: Variable t) = do
  (SomeVariable (value :: k)) <- Map.lookup i (values holder)
  Refl <- eqT @t @k
  pure value

-- |Register new variable with its value and return it
addVariable :: (Show a, Typeable a) => VarHolder -> a -> (Variable a, VarHolder)
addVariable holder newVar = do
  let
    newLastIndex = lastIndex holder + 1
    newValues = Map.insert (lastIndex holder) (SomeVariable newVar) (values holder)
  (Variable (lastIndex holder) :: Variable a, VarHolder newValues newLastIndex)

-- |Apply function on variable value by variable
updateVariable :: (Show a, Typeable a) => VarHolder -> Variable a -> (a -> a) -> VarHolder
updateVariable holder var f =
  let
    newValues = do
      oldValue <- getVariable holder var
      let newValue = f oldValue
      return $ Map.insert (vindex var) (SomeVariable newValue) (values holder)
  in
    if (isJust newValues)
    then holder { values = fromJust newValues }
    else holder

-- |Interprets expression using given VarHolder
interpretExpr :: Show a => VarHolder -> ArithExpr a -> a
interpretExpr _ (AENum n) = n
interpretExpr _ (AEBool b) = b
interpretExpr _ (AEString s) = s
interpretExpr vars (AENot e) = not $ interpretExpr vars e
interpretExpr vars (AEPlus a b) = interpretExpr vars a + interpretExpr vars b
interpretExpr vars (AEMul a b) = interpretExpr vars a * interpretExpr vars b
interpretExpr vars (AEDiv a b) = interpretExpr vars a `div` interpretExpr vars b
interpretExpr vars (AEMod a b) = interpretExpr vars a `mod` interpretExpr vars b
interpretExpr vars (AESub a b) = interpretExpr vars a - interpretExpr vars b
interpretExpr vars (AEAnd a b) = interpretExpr vars a && interpretExpr vars b
interpretExpr vars (AEGt a b) = interpretExpr vars a > interpretExpr vars b
interpretExpr vars (AEEq a b) = interpretExpr vars a == interpretExpr vars b
interpretExpr vars (AEConcat a b) = interpretExpr vars a ++ interpretExpr vars b
interpretExpr vars (AEVar var) = fromJust $ getVariable vars var
interpretExpr vars (ToString e) = show $ interpretExpr vars e

-- |Testing expression, makes some meaningless pow using all three types (Bool, String, Int)
--To run: (callHalyavaFunction2 halyavaOddPow :: Int -> Int -> Maybe String) 1 1
--It is important to directly specify types, or it wouldn't work
halyavaOddPow :: HalyavaScriptExpr
halyavaOddPow =
  Fun2 (\a b ->
    WithVar 0 (\res ->
      res @= AENum 1 #
      WithVar "" (\lg ->
        lg @= ToString (AEVar a) `AEConcat` AEString " ^ " `AEConcat` ToString (AEVar b) `AEConcat` AEString " = 1" #
        While (AEVar b `AEGt` AENum 0) (
          b @= AEVar b `AESub` AENum 1 #
          res @= AEVar res `AEMul` AEVar a #
          lg @= AEVar lg `AEConcat` AEString " * " `AEConcat` ToString (AEVar a)
        )#
        If (AEVar res `AEGt` AENum 1000) (
          WithVar False (\isOdd ->
            isOdd @= AEVar res `AEMod` AENum 2 `AEEq` AENum 1 #
            If (AENot (AEVar isOdd)) (
              lg @= AEVar lg `AEConcat` AEString " |!!!| Result is not odd and greater then 1000! Fixing! |!!!|"
            )#
            While (AENot (AEVar isOdd)) (
              res @= AEVar res `AEDiv` AENum 2 #
              lg @= AEVar lg `AEConcat` AEString " / 2" #
              isOdd @= AEVar res `AEMod` AENum 2 `AEEq` AENum 1
            )
          )
        )#
        lg @= AEVar lg `AEConcat` AEString " = " `AEConcat` ToString (AEVar res) #
        Return (AEVar lg)
      )
    )
  )

-- |Data representation of HalyavaScript using GADT
--Fun1 and Fun2 works like if they had return before them
data HalyavaScriptExpr where
  VarEquating :: (Typeable a, Show a) => Variable a -> ArithExpr a -> HalyavaScriptExpr
  Fun1 :: (Typeable a, Show a) => (Variable a -> HalyavaScriptExpr) -> HalyavaScriptExpr
  Fun2 :: (Typeable a, Show a, Typeable b, Show b) => (Variable a -> Variable b -> HalyavaScriptExpr) -> HalyavaScriptExpr
  WithVar :: (Typeable a, Show a, HalyavaShow a) => a -> (Variable a -> HalyavaScriptExpr) -> HalyavaScriptExpr
  LineUnion :: HalyavaScriptExpr -> HalyavaScriptExpr -> HalyavaScriptExpr
  Return :: (Typeable a, Show a) => ArithExpr a -> HalyavaScriptExpr
  While :: ArithExpr Bool -> HalyavaScriptExpr -> HalyavaScriptExpr
  If :: ArithExpr Bool -> HalyavaScriptExpr -> HalyavaScriptExpr

-- |Return type of interpreting halyava script expression
data Something where
  Something :: Typeable a => a -> Something

-- |Converts result into given type
--It's important to specify the type
fromSomething :: forall a. Typeable a => Something -> Maybe a
fromSomething (Something (val :: t)) = do
  Refl <- eqT @t @a
  pure val

-- |Intrpretating given halyava script function1, turning it into haskell fucntion
--It is a helper function to manage halyava script functions better
--It's important to specify the type
--Will fail on non halyava script functions
interpretHalyavaFunction1 :: forall t. Typeable t => HalyavaScriptExpr -> (t -> Maybe Something)
interpretHalyavaFunction1 fun = \(x :: t) ->
  fromJust (fromSomething $ fromJust $ interpretHalyavaScript fun :: Maybe (t -> Maybe Something)) x :: Maybe Something

-- |Intrpretating given halyava script function1, turning it into haskell fucntion but even better
--It is a helper function to manage halyava script functions better
--It's important to specify the type
--Will fail on non halyava script functions
callHalyavaFunction1 :: forall t a. (Typeable a, Typeable t) => HalyavaScriptExpr -> t -> Maybe a
callHalyavaFunction1 f arg = fromSomething $ fromJust $ ((interpretHalyavaFunction1 f) :: t -> Maybe Something) arg :: Maybe a

-- |Intrpretating given halyava script function2, turning it into haskell fucntion but even better
--It is a helper function to manage halyava script functions better
--It's important to specify the types
--Will fail on non halyava script functions
interpretHalyavaFunction2 :: forall t k. (Typeable t, Typeable k) => HalyavaScriptExpr -> (t -> k -> Maybe Something)
interpretHalyavaFunction2 fun = \(x :: t) (y :: k) ->
  fromJust (fromSomething $ fromJust $ interpretHalyavaScript fun :: Maybe (t -> k -> Maybe Something)) x y :: Maybe Something

-- |Intrpretating given halyava script function2, turning it into haskell fucntion but even better
--It is a helper function to manage halyava script functions better
--It's important to specify the types
--Will fail on non halyava script functions
callHalyavaFunction2 :: forall t k a. (Typeable a, Typeable t, Typeable k) => HalyavaScriptExpr -> t -> k -> Maybe a
callHalyavaFunction2 f arg1 arg2 = fromSomething $ fromJust $ ((interpretHalyavaFunction2 f) :: t -> k -> Maybe Something) arg1 arg2 :: Maybe a

-- |Interprets and unwraps exestential
-- fails on no returs function, or if type specified incorrectly
interpretAndUnwrap :: forall a. Typeable a => HalyavaScriptExpr -> a
interpretAndUnwrap expr = fromJust $ fromSomething $ fromJust $ interpretHalyavaScript expr :: a

-- |General interpret
--Executes given halyava script
--Returns result as a Something
--You need to know/specify types of the halyava script function, if you want to unwrap the value
interpretHalyavaScript :: HalyavaScriptExpr -> Maybe Something
interpretHalyavaScript = interpretHalyavaScriptHolder emptyHolder

-- |Interpret, but you can pass some variables in
interpretHalyavaScriptHolder :: VarHolder -> HalyavaScriptExpr -> Maybe Something
interpretHalyavaScriptHolder variables expression =
  flip evalState variables $ interpretHalyavaScriptState expression
  where
    interpretHalyavaScriptState :: HalyavaScriptExpr -> State VarHolder (Maybe Something)
    interpretHalyavaScriptState (VarEquating var expr) = do
      holder <- get
      put $ updateVariable holder var (const (interpretExpr holder expr))
      return $ Nothing
    interpretHalyavaScriptState (LineUnion l1 l2) = do
      f1 <- interpretHalyavaScriptState l1
      f2 <- interpretHalyavaScriptState l2
      return $ if (isJust f1) then f1 else f2
    interpretHalyavaScriptState (Return expr) = do
      holder <- get
      return $ pure $ Something $ interpretExpr holder expr
    interpretHalyavaScriptState (WithVar initVal scope) = do
      holder <- get
      let (newVar, newHolder) = addVariable holder initVal
      put newHolder
      interpretHalyavaScriptState (scope newVar)
    interpretHalyavaScriptState (If condition thenCode) = do
      holder <- get
      let isCondition = interpretExpr holder condition
      if isCondition
      then interpretHalyavaScriptState thenCode
      else return Nothing
    interpretHalyavaScriptState (While condition scope) = do
      holder <- get
      let isCondition = interpretExpr holder condition
      if isCondition
      then do
        f1 <- interpretHalyavaScriptState scope
        if (isJust f1)
        then return f1
        else interpretHalyavaScriptState (While condition scope)
      else return Nothing
    interpretHalyavaScriptState (Fun1 f) = do
      holder <- get
      return $ pure $ Something $ \x -> do
        let (varX, newHolder) = addVariable holder x
        flip evalState newHolder $ interpretHalyavaScriptState (f varX)
    interpretHalyavaScriptState (Fun2 f) = do
      holder <- get
      return $ pure $ Something $ \x y -> do
        let
          (varX, newHolder1) = addVariable holder x
          (varY, newHolder2) = addVariable newHolder1 y
        flip evalState newHolder2 $ interpretHalyavaScriptState (f varX varY)

-- |Function representing the endline (LineUnion)
infixl 7 #
(#) :: HalyavaScriptExpr -> HalyavaScriptExpr -> HalyavaScriptExpr
(#) line1 line2 = LineUnion line1 line2

-- |Function representing VarEquating
infixr 8 @=
(@=) :: (Typeable a, Show a) => Variable a -> ArithExpr a -> HalyavaScriptExpr
(@=) var expr = VarEquating var expr
