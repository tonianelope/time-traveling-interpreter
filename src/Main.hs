{-# Language FlexibleContexts, NoMonomorphismRestriction#-} 
module Main where

import Lang
import Prelude hiding (lookup)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import qualified Text.Read as R
import qualified Data.Map as Map

data Command = Break Expr | R | C | S | H | L | Clear deriving (Read, Eq)

type Step = (Env, Statement)
-- IState : List of Steps taken, current Env, List of Breapoints, Current Run Command
type IState = ([Step], Env, [Expr], Command)
type Run a = StateT IState (ExceptT String IO) a

type Program = [Statement]

-- default interpreter file
prgFile = "./src/tti.prg"
helpText = "\tCMD - useage \n"
  ++ "\tH  - displays help \n"
  ++ "\tS  - step through program \n"
  ++ "\tC  - continue programm \n"
  ++ "\tR  - reverse/ step back to last state change\n"
--  ++ "               P  - print proram position"
  ++ "\tBreak (Bool Expr) - set breakpoint \n"

isBoo :: Expr -> Bool
isBoo (Or _ _) = True
isBoo (And _ _) = True
isBoo (Gt _ _) = True
isBoo (Lt _ _) = True
isBoo (Eq _ _) = True
isBoo _ = False

-- MODIFY ISTATE Functions
setStep :: (Name, Val) -> Statement -> IState -> IState
setStep (n, v) s (past, env, br, cmd) = ((env, s):past, Map.insert n v env, br, cmd)



moveBack :: Env -> IState -> IState
moveBack env (p:ps, _, br, cmd) = (ps, env, br, cmd)

stateEnv :: IState -> Env
stateEnv (_, env ,_, _) = env

statePast :: IState -> [Step]
statePast (s, _, _, _) = s

stateBreakpoints :: IState -> [Expr]
stateBreakpoints (_, _, br, _) = br

stateCmd :: IState -> Command
stateCmd (_,_,_,cmd) = cmd

setBreak :: IState -> Expr -> IState
setBreak (p, f, brs, cmd) br = (p, f, br:brs, cmd)

clearBreak :: IState -> IState
clearBreak (p, f, _, cmd) = (p, f, [], cmd)

setCmd :: Command -> IState -> IState
setCmd cmd (p, env, brs, _) = (p, env, brs, cmd)

-- EXEC Statements
exec :: Statement -> Run ()
exec Pass = return ()
exec (Seq s0 s1) = exec s0 >> flowControl s1
exec (Assign s v) = do
  env <- gets stateEnv
  case runEval env (eval v) of
    Right val -> modify $ setStep (s,val) (Assign s v)
    Left msg -> throwError msg

exec (Print e) = do
  env <- gets stateEnv
  case runEval env (eval e) of
    Right val -> liftIO $ print val
    Left msg -> throwError msg

exec (If ex st sf) = do
  env <- gets stateEnv
  case runEval env (eval ex) of
    Right (B True) -> flowControl st
    Right _ -> flowControl sf
    Left msg -> throwError msg

exec (While ex stm) = do
  env <- gets stateEnv
  case runEval env (eval ex) of
    Right (B True) -> flowControl $ stm `Seq` (While ex stm)
    Right (B False) -> return ()
    Left msg -> throwError msg

exec (Try try exept) = do
  exec try `catchError` (\msg -> (liftIO $ putStrLn msg) >> flowControl exept)
  return ()

execReverse :: Statement -> Run ()
execReverse stm = do
  past <- gets statePast
  case past of
    ((env, s):_) -> (modify $ moveBack env) >> getRunCmd (Seq s stm)
    _ -> (liftIO $ putStrLn "At the beginnig of the program - can't step back") >> getRunCmd stm

-- check flow control
--   if breakpoint break
--   if cmd is step break
--   else run
flowControl :: Statement -> Run ()
flowControl s = do
  br <- gets stateBreakpoints
  env <- gets stateEnv
  let bools = map eval br
  if any (evalIsTrue env) (map eval br)
    then getRunCmd s
    else do
           cmd <- gets stateCmd
           case cmd of
             S -> getRunCmd s
             _ -> exec s

evalIsTrue :: Env -> Eval Val -> Bool
evalIsTrue env ex = case runEval env ex of
  Right (B True) -> True
  _ -> False

execCommand :: Command -> Statement -> Run ()
execCommand (Break e) s = (if isBoo e
                           then modify (`setBreak` e)
                           else liftIO $ putStrLn "Not valid boo")
                          >> getRunCmd s
execCommand L s = do
  brs <- gets stateBreakpoints
  liftIO $ mapM_ print brs
  getRunCmd s
execCommand Clear s = (modify clearBreak) >> getRunCmd s
execCommand R s = execReverse s
execCommand H s = (liftIO $ putStrLn helpText) >> getRunCmd s
execCommand c s = (modify $ setCmd c) >> exec s

getBaseStatement :: Statement -> Statement
getBaseStatement (Seq a _) = getBaseStatement a
getBaseStatement (If expr _ _) = (If expr Pass Pass)
getBaseStatement (While expr _) = (While expr Pass)
getBaseStatement (Try a _) = getBaseStatement a
getBaseStatement a = a

printState :: Env -> Statement -> [Step] -> IO ()
printState env stm past = do
    case past of
      [] -> putStr ""
      (_, prev):_ -> putStr " past >> " >> print prev
    putStr "State: "
    print $ Map.toList env
    putStr " next >> " >> (print $ getBaseStatement stm)

getRunCmd :: Statement -> Run ()
getRunCmd prg = do
  env <- gets stateEnv
  past <- gets statePast
  liftIO $ printState env prg past >> putStr "> "
  line <- liftIO $ R.readMaybe <$> getLine
  case line of
    (Just c) -> execCommand c prg
    _ -> (liftIO $ putStrLn "Invalid input - use H for help") >> getRunCmd prg

compile :: Program -> Statement
compile p = foldr Seq Pass p

runInterpreter :: Program -> IO ()
runInterpreter p = do
  result <- runExceptT $ (runStateT  (getRunCmd (compile p)) ([], Map.empty, [], C) )
  case result of
    Right ((), env) -> return ()
    Left exn -> print ("Uncaught exception: "++exn)

getProgramm :: [String] -> String
getProgramm [] = prgFile
getProgramm (path:_) = path

readProg :: FilePath -> IO (Maybe Program)
readProg = fmap R.readMaybe . readFile

main = do
  hSetBuffering stdout NoBuffering -- fix IO buffering when compiling
  s <- getArgs
  let file = getProgramm s
  putStrLn $ "Program: " ++ file
  mp <- readProg file
  case mp of
    Just prg -> runInterpreter prg -- $ read "test.prg"
    Nothing -> putStrLn "Couldn't parse file"
