module Block5.Task1 ( Expr (..)
                    ,  BinaryExpr (..)
                    , ArithmeticError (..)
                    , eval
                    ) where

-- |Type representing mathematical expression
data Expr = Value Int
          | Binary BinaryExpr
          deriving (Show)

-- |Binary Mathematical operation
data BinaryExpr = Add Expr Expr
                | Sub Expr Expr
                | Mul Expr Expr
                | Div Expr Expr
                | Pow Expr Expr
                deriving (Show)

-- |Error during calculation of expression
data ArithmeticError = DivisionByZero
                     | NegativePow
                     deriving (Show, Eq)

-- |Returns function to calculate result of given binary expression and
-- both, left and right operands
getOp :: BinaryExpr -> ((Int -> Int -> Either ArithmeticError Int), Expr, Expr)
getOp (Add a b) = (\x -> return . (+x), a, b)
getOp (Sub a b) = (\x -> return . (x-), a, b)
getOp (Mul a b) = (\x -> return . (*x), a, b)
getOp (Div a b) =
  ( \x y ->
    if y == 0
    then Left DivisionByZero
    else return (x `div` y)
  , a, b)
getOp (Pow a b) =
  ( \x y ->
    if y < 0
    then Left NegativePow
    else return (x^y)
  , a, b)

-- |Evaluates given expression and returns Either holding
-- ArithmeticError of result
eval :: Expr -> Either ArithmeticError Int
eval (Value x) = Right x
eval (Binary binaryOp) =
  let (op, left, right) = getOp binaryOp in do
    leftRes <- eval left
    rightRes <- eval right
    op leftRes rightRes
