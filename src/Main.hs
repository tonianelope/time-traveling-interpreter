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

type Step = (Env, Statement)
-- past  future??
type Location = ([Step], [Step])
type IState = ([Step], Env, [Expr])
type Run a = StateT IState (ExceptT String IO) a

-- setEnv :: (Name, Val) -> Run ()
-- setEnv (s,i) = state $ (\ (_, )
--                            (table:tables, br) -> (() , ((Map.insert s i table):table:tables, br ) ) )

setStep :: (Name, Val) -> Statement -> IState -> IState
setStep (n, v) s (past, env, br) = ((env, s):past, new_env, br) where new_env = Map.insert n v env

moveBack :: Env -> IState -> IState
moveBack env (p:ps, _, br) = (ps, env, br)

getEnv :: IState -> Env
getEnv (_, env ,_) = env

getPast :: IState -> [Step]
getPast (s, _, _) = s

getBreakpoints :: IState -> [Expr]
getBreakpoints (_, _, br) = br

setBreak :: IState -> Expr -> IState
setBreak (p, f, brs) br = (p, f, br:brs)
--br = state $ (\(env, brs) -> ((), (env, br:brs)))

exec :: Statement -> Run ()
exec (Seq s0 s1) = do exec s0 >> checkBreak s1
exec (Assign s v) = do
  liftIO $ System.print (Assign s v)
  env <- gets getEnv
  Right val <- return $ runEval env (eval v)
  modify $ setStep (s,val) (Assign s v)

exec (Print e) = do
  liftIO $ System.print (Print e)
  env <- gets getEnv
  Right val <- return $ runEval env (eval e)
  liftIO $ System.print val

exec (If ex st sf) = do
  env <- gets getEnv
--  Right val <- return $ runEval env exp
  case runEval env (eval ex) of
    Right (B True) -> exec st
    _ -> exec sf

exec (While ex stm) = do
  env <- gets getEnv
  case runEval env (eval ex) of
    Right (B True) -> exec stm >> checkBreak (While ex stm)
    _ -> return ()

execReverse :: Statement -> Run ()
execReverse stm = do
  past <- gets getPast
  case past of
    ((env, s):_) -> (modify $ moveBack env) >> interpret (Seq s stm)
    _ -> (liftIO $ putStrLn "At the beginnig of the program - can't step back") >> interpret stm

  -- case a of
  --  Right stm -> interpret s
  --  Left msg -> (liftIO $ print msg) >> interpret s

--_ (\(env, stm) -> (modify $ setEnv env) >> interpret (Seq stm s))

checkBreak :: Statement -> Run ()
checkBreak s = do
  br <- gets getBreakpoints
  --[Eval Val]
  env <- gets getEnv
  let bools = map eval br
  if any (brEvalIsTrue env) (map eval br) then interpret s
    else interpret s

brEvalIsTrue :: Env -> Eval Val -> Bool
brEvalIsTrue env ex = case runEval env ex of
  Right (B True) -> True
  _ -> False

execCommand :: Command -> Statement -> Run ()
execCommand C s = exec s
execCommand (Break e) s = (if isBoo e
                           then modify (`setBreak` e)
                           else liftIO $ putStrLn "Not valid boo")
                          >> interpret s
execCommand R s = execReverse s
-- execCommand S s = TODO step

interpret :: Statement -> Run ()
interpret prg = do
  env <- gets getEnv
  liftIO $ putStrLn "State:"
  liftIO $ print env
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
  result <- runExceptT $ (runStateT  (interpret (compile p)) ([], Map.empty, []) )
  return ()
  -- case result of
  --   Right ((), env) -> return ()
  --   Left exn -> System.print ("Uncaught exception: "++exn)

prog :: Program
prog = do
  tell $ (Assign "i" (Const (I 0)))
  tell $ (While (Lt (Var "i") (Const (I 5)))
          (Assign "i" (Add (Var "i") (Const (I 2))))
          )
  tell $ (Print (Var "i"))

-- TODO handle error
  -- tell $ (Assign "arg" (Const (I 0)))
  -- tell $ (Assign "scratch" (Var "arg"))
  -- tell $ (Assign "arg" (Add (Var "arg") (Const (I 5))))
  -- tell $ (If (Lt (Const (I 15)) (Var "arg"))
  --        (Assign "arg" (Const (I 8)))
  --         (Assign "arg" (Const (I 15))))
  -- tell $ (Assign "arg" (Const (I 8)))
  -- tell $ (Assign "test" (Sub (Var "arg") (Const (I 7))))

testprog :: Program
testprog = read "test.prg"

data Command = Break Expr | R | C | S deriving Read

-- TODO optional read instances
-- instance Read Command where
--   readS = 

-- type Breakpoint
isBoo :: Expr -> Bool
isBoo (Or _ _) = True
isBoo (And _ _) = True
isBoo (Gt _ _) = True
isBoo (Lt _ _) = True
isBoo (Eq _ _) = True
isBoo _ = False

main = run prog
