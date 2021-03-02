{-# LANGUAGE GADTs #-}

module Task4 ( showExpr, showHalyavaExpr) where

import Control.Monad.State.Lazy ( State, get, put, evalState)

import Task3 ( HalyavaScriptExpr (..), ArithExpr (..), Variable (..), HalyavaShow (..) )

addBrackets :: String -> String
addBrackets s = "(" ++ s ++ ")"

-- |Pretty prints arith expression
showExpr :: ArithExpr a -> String
showExpr (AENum n) = hShow n
showExpr (AEBool b) = hShow b
showExpr (AEString s) = hShow s
showExpr (AENot e) = "!" ++ showExpr e
showExpr (AEPlus a b) = addBrackets $ showExpr a ++ " + " ++ showExpr b
showExpr (AEMul a b) = addBrackets $ showExpr a ++ " * " ++ showExpr b
showExpr (AEDiv a b) = "Math.floor(" ++ showExpr a ++ " / " ++ showExpr b ++")"
showExpr (AEMod a b) = addBrackets $ showExpr a ++ " % " ++ showExpr b
showExpr (AESub a b) = addBrackets $ showExpr a  ++ " - " ++ showExpr b
showExpr (AEAnd a b) = addBrackets $ showExpr a ++ " && " ++ showExpr b
showExpr (AEGt a b) = addBrackets $ showExpr a ++ " > " ++ showExpr b
showExpr (AEEq a b) = addBrackets $ showExpr a ++ " == " ++ showExpr b
showExpr (AEConcat a b) = addBrackets $ showExpr a ++ " + " ++ showExpr b
showExpr (AEVar var) = showVariable var
showExpr (ToString e) = addBrackets $ "\"\" + " ++ showExpr e

showVariable :: Variable a -> String
showVariable (Variable index) = "var" ++ show index

data ShowExprState = ShowExprState { indent :: Int
                                   , lastVar :: Int
                                   }

printIndent :: State ShowExprState String
printIndent = do
  s <- get
  return $ [1 .. indent s] >> "  "

-- |Pretty prints halyava script expression
showHalyavaExpr :: HalyavaScriptExpr -> String
showHalyavaExpr e = "(() => {\n" ++ (flip evalState (ShowExprState 1 0) $ showHalyavaExprHelper e) ++ "\n})();"
  where
    showHalyavaExprHelper :: HalyavaScriptExpr -> State ShowExprState String
    showHalyavaExprHelper (VarEquating var expr) = do
      ind <- printIndent
      return $ ind ++ showVariable var ++ " = " ++ showExpr expr
    showHalyavaExprHelper (Fun1 f) = do
      s <- get
      let
        newVar = Variable $ lastVar s
        applyedF = f newVar
      ind <- printIndent
      put $ s { indent = indent s + 1, lastVar = lastVar s + 1 }
      fShown <- showHalyavaExprHelper applyedF
      put $ s { lastVar = lastVar s + 1}
      return $ ind ++ "return function (" ++ showVariable newVar ++ ") {\n" ++ fShown ++ "\n" ++ ind ++"}"
    showHalyavaExprHelper (Fun2 f) = do
      s <- get
      let
        newVar1 = Variable $ lastVar s
        newVar2 = Variable $ lastVar s + 1
        applyedF = f newVar1 newVar2
      ind <- printIndent
      put $ s { indent = indent s + 1, lastVar = lastVar s + 2 }
      fShown <- showHalyavaExprHelper applyedF
      put $ s { lastVar = lastVar s + 2 }
      return $ ind ++ "return function (" ++ showVariable newVar1 ++ ", " ++ showVariable newVar2 ++ ") {\n" ++ fShown ++ "\n" ++ ind ++ "}"
    showHalyavaExprHelper (WithVar value f) = do
      s <- get
      let
        newVar = Variable $ lastVar s
        applyedF = f newVar
      ind <- printIndent
      put $ s { lastVar = lastVar s + 1 }
      fShown <- showHalyavaExprHelper applyedF
      return $ ind ++ "var " ++ showVariable newVar ++ " = " ++ hShow value ++ ";\n" ++ fShown
    showHalyavaExprHelper (LineUnion f s) = do
      fShown <- showHalyavaExprHelper f
      sShown <- showHalyavaExprHelper s
      return $ fShown ++ ";\n" ++ sShown
    showHalyavaExprHelper (Return expr) = do
      ind <- printIndent
      return $ ind ++ "return " ++ showExpr expr
    showHalyavaExprHelper (While cond scoped) = do
      s <- get
      ind <- printIndent
      put $ s { indent = indent s + 1 }
      scopedShown <- showHalyavaExprHelper scoped
      put s
      return $ ind ++ "while (" ++ showExpr cond ++") {\n" ++ scopedShown ++ "\n" ++ ind ++"}"
    showHalyavaExprHelper (If cond scoped) = do
      s <- get
      ind <- printIndent
      put $ s { indent = indent s + 1 }
      scopedShown <- showHalyavaExprHelper scoped
      put s
      return $ ind ++ "if (" ++ showExpr cond ++") {\n" ++ scopedShown ++ "\n" ++ ind ++ "}"
