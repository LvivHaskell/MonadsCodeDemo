module Lib
    ( Expr(..)
    , eval
    ) where

import           Control.Monad (guard)

data Expr
  = Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Const Int
  deriving Show

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

checkedF :: (Integer -> Integer -> Integer) -> Int -> Int -> Maybe Int
checkedF f a b =
  let res = f (toInteger a) (toInteger b)
  in
    guard (res >= toInteger (minBound :: Int) && res <= toInteger (maxBound :: Int)) >>
    Just (fromIntegral res)

maybeEval
  :: (Integer -> Integer -> Integer)
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
maybeEval f a b =
  a >>= (\resultA ->
  b >>= (\resultB ->
  checkedF f resultA resultB
  ))

eval :: Expr -> Maybe Int
eval (a :+: b) = maybeEval (+) (eval a) (eval b)
eval (a :-: b) = maybeEval (-) (eval a) (eval b)
eval (a :*: b) = maybeEval (*) (eval a) (eval b)
eval (a :/: b) =
  eval b >>= (\resultB ->
    if resultB == 0
    then Nothing
    else maybeEval div (eval a) (Just resultB)
  )
eval (Const n) = Just n
