{-# Language FlexibleContexts, NoMonomorphismRestriction#-} 
module Main where

import Prelude hiding (lookup)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Text.Read as R
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
                         _            -> fail "type error in arithmetic expression"

--Boolean typed expressions
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

--Operations over integers which produce booleans
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

--Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


-- RUN STUFF
type Run a = StateT ([Env], [Expr]) (ExceptT String IO) a

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\(table:tables, br) -> (() , ((Map.insert s i table):tables, br ) ) )

setBreak :: Expr -> Run ()
setBreak br = state $ (\(env, brs) -> ((), (env, br:brs)))

exec :: Statement -> Run ()
exec (Seq s0 s1) = do exec s0 >> isBreak s1
exec (Assign s v) = do
  liftIO $ System.print (Assign s v)
  (st:sts, br) <- get
  Right val <- return $ runEval st (eval v)
  set (s,val)

exec (Print e) = do
  liftIO $ System.print (Print e)
  (st:sts, br) <- get
  Right val <- return $ runEval st (eval e)
  liftIO $ System.print val

isBreak :: Statement -> Run ()
isBreak s = do
  (st:sts, br) <- get
  --[Eval Val]
  let bools = map eval br
  if any (isTrue st ) bools then interpret s
    else interpret s

isTrue :: Env -> Eval Val -> Bool
isTrue env ex = case runEval env ex of
  Right (B True) -> True
  _ -> False

execCommand :: Command -> Statement -> Run ()
execCommand C s = exec s >> (liftIO $ print "done")
execCommand (Break e) s = (if isBoo e then setBreak e else liftIO $ putStrLn "Not valid boo") >> interpret s

interpret :: Statement -> Run ()
interpret prg = do
  (st:sts, br) <- get
  liftIO $ putStrLn "State:"
  liftIO $ print st
  liftIO $ putStr "> "
  line <- liftIO $ R.readMaybe <$> getLine
  case line of
    (Just c) -> execCommand c prg
    _ -> (liftIO $ putStrLn "Invalid input") >> interpret prg
  --let m = Map.toList st
  --liftIO $ mapM_ System.print m

-- PROGRAM STUFF Writer a ()
type Program = Writer Statement ()

instance Semigroup Statement where
  a <> b = a `Seq` b

instance Monoid Statement where
  mempty = Pass
  mappend = (<>)

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

-- note exec??
run :: Program -> IO ()
run p = do
  let res = (compile p)
  print res
  result <- runExceptT $ (runStateT  (interpret (compile p)) ([Map.empty], []) )
  return ()
  -- case result of
  --   Right ((), env) -> return ()
  --   Left exn -> System.print ("Uncaught exception: "++exn)

prog :: Program
prog = do
  tell $ (Assign "arg" (Const (I 10)))
  tell $ (Assign "scratch" (Var "arg"))
  tell $ (Assign "arg" (Add (Var "arg") (Const (I 5))))
  tell $ (Assign "test" (Sub (Var "arg") (Const (I 7))))

testprog :: Program
testprog = read "test.prg"

data Command = Break Expr | R | C

instance Read Command where
  readS = 

-- type Breakpoint
isBoo :: Expr -> Bool
isBoo (Or _ _) = True
isBoo (And _ _) = True
isBoo (Gt _ _) = True
isBoo (Lt _ _) = True
isBoo (Eq _ _) = True
isBoo _ = False

main = run prog
