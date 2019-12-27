module Lib
    ( Expr(..)
    , EvalError(..)
    , eval
    , toNats
    , State(..)
    ) where

import           Control.Monad (ap, liftM, when)

data Expr
  = Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Const Int
  deriving Show

data EvalError = OverflowError | DivisionByZero
  deriving Show

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

checkedF :: (Integer -> Integer -> Integer) -> Int -> Int -> Either EvalError Int
checkedF f a b =
  let res = f (toInteger a) (toInteger b)
  in
    when
      (res < toInteger (minBound :: Int) || res > toInteger (maxBound :: Int))
      (Left OverflowError) >>
    return (fromIntegral res)

maybeEval
  :: (Integer -> Integer -> Integer)
  -> Either EvalError Int
  -> Either EvalError Int
  -> Either EvalError Int
maybeEval f a b =
  a >>= (\resultA ->
  b >>= (\resultB ->
  checkedF f resultA resultB
  ))

eval :: Expr -> Either EvalError Int
eval (a :+: b) = maybeEval (+) (eval a) (eval b)
eval (a :-: b) = maybeEval (-) (eval a) (eval b)
eval (a :*: b) = maybeEval (*) (eval a) (eval b)
eval (a :/: b) =
  eval b >>= (\resultB ->
    if resultB == 0
    then Left DivisionByZero
    else maybeEval div (eval a) (return resultB)
  )
eval (Const n) = return n

newtype State s a = State { runState :: s -> (a, s) }

getState :: State s s
getState = State (\s -> (s, s))

updateState :: s -> State s ()
updateState newS = State (\_ -> ((), newS))

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State (\s -> (x, s))
  (>>=) (State a) f = State (\s ->
    let (resA, newS) = a s
        State ff = f resA
    in ff newS)

toNats' :: (Expr -> Expr -> Expr) -> Expr -> Expr -> State Int Expr
toNats' f a b =
  toNats a >>= (\resA ->
  toNats b >>= (\resB ->
  return (f resA resB)
  ))

toNats :: Expr -> State Int Expr
toNats (Const _) =
  getState >>= (\lastUnused ->
  updateState (lastUnused + 1) >> (
  return (Const lastUnused)
  ))
toNats (a :+: b) = toNats' (:+:) a b
toNats (a :-: b) = toNats' (:-:) a b
toNats (a :*: b) = toNats' (:*:) a b
toNats (a :/: b) = toNats' (:/:) a b

{-
 - Monad - вычисление с побочным эффектом
 -
 - Maybe - вычисление, которое может не завершиться
 - Either - вычисление, которое может завершиться с ошибкой конкретного типа
 - State - вычисление с состоянием из одной переменной
 - IO - вычисление, взаимодействующее с внешним миром (State RealWorld)
 - Writer - вычисление, записывающее что-то в лог
 - Reader - вычисление, которое может читать из неизменяемого контекста
 - [] - недетерминированное вычисление
 -
 - Parser - монада для
 - (библиотеки Parsec, Megaparsec)
 -
 - Cont - continuations (обобщение всяких async/await, coroutines, через это
 - можно выразить много интересных конструкций, но на практике применяется
 - редко)
 -
 - Reader + IO - часто используются в комбинации для работы с внешним миром
 -}
