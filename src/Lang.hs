{-# Language FlexibleContexts, NoMonomorphismRestriction#-} 
module Lang where

import Prelude hiding (lookup)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified System.IO as System



data Val = I Int | B Bool
  deriving (Eq, Show, Read)

data Expr = Const Val
  | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
  | And Expr Expr | Or Expr Expr | Not Expr
  | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
  | Var String
  deriving (Eq, Show, Read)

-- statements
data Statement = Assign String Expr
                | If Expr Statement Statement
                | While Expr Statement
                | Print Expr
                | Seq Statement Statement
                | Try Statement Statement
                | Pass
       deriving (Eq, Show, Read)

type Name = String
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> throwError ("Unkwon variable " ++ k)

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval env ex = runIdentity (runExceptT (runReaderT ex env))

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> throwError "type error in arithmetic expression"

--Boolean typed expressions
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> throwError "type error in boolean expression"

--Operations over integers which produce booleans
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> throwError "type error in arithmetic expression"

--Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0 ) = do evalb (const not) (Const (B True)) e0

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env
