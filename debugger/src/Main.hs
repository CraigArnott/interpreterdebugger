{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

import qualified System.IO as System

import Text.Read (readMaybe) 
import Data.Char (toUpper)

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- Our mainline                                                    -}
{-------------------------------------------------------------------}
 
main = runInterpreter

runInterpreter :: IO ()
runInterpreter = do
    contents <- readFile "myFile.txt"
    let program = (read contents :: Statement)
    run program
    putStr $ "\n"
 
{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}
   
data Val = I Int | B Bool
    deriving (Eq, Show, Read)

data Expr = Const Val
         | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
         | And Expr Expr | Or Expr Expr | Not Expr 
         | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
         | Var String
    deriving (Eq, Show, Read)

type Name = String 
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
    Just x -> return x
    Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator, 
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a 

runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

--Integer typed expressions

evali op e0 e1 = do 
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ I (i0 `op` i1)
        _ -> fail "type error in arithmetic expression"

--Boolean typed expressions

evalb op e0 e1 = do 
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (B i0, B i1) -> return $ B (i0 `op` i1)
        _ -> fail "type error in boolean expression"

evalib op e0 e1 = do
    e0' <- eval e0
    e1' <- eval e1
    case (e0', e1') of
        (I i0, I i1) -> return $ B (i0 `op` i1)
        _ -> fail "type error in arithmetic expression"

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
                        
eval (Var s) = do 
    env <- ask
    lookup s env

{-------------------------------------------------------------------}
{- The statement language                                          -}
{-------------------------------------------------------------------}

data Statement = Assign String Expr
                | If Expr Statement Statement
                | While Expr Statement
                | Print Expr
                | Seq Statement Statement
                | Try Statement Statement
                | Pass                    
    deriving (Eq, Show, Read)

run :: Statement -> IO ()
run s = do
    result <- runExceptT $ (runStateT $ exec s) Map.empty
    case result of
        Right _ -> return ()
        Left e -> System.print ("Uncaught exception: " ++ e)

type Run a = StateT Env (ExceptT String IO) a 
runRun p =  runExceptT (runStateT p Map.empty) 

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table -> ((), Map.insert s i table))

exec :: Statement -> Run ()

exec (Assign s v) = do
    st <- get  
    Right val <- return $ runEval st (eval v)  
    set (s,val)

exec (Seq s0 s1) = do presentMenu s0 >> presentMenu s1

exec (Print e) = do 
    st <- get
    Right val <- return $ runEval st (eval e) 
    liftIO $ System.print val
    return () 

exec (If cond s0 s1) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    if val then do presentMenu s0 else do presentMenu s1

exec (While cond s) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    if val then do presentMenu s >> presentMenu (While cond s) else return ()

exec (Try s0 s1) = do catchError (presentMenu s0) (\e -> presentMenu s1)

exec Pass = return ()

{-------------------------------------------------------------------}
{- Logic for the options menu                                      -}
{-------------------------------------------------------------------}

data Command = N
             | Inspect String
    deriving (Show, Read, Eq)

peekVar :: String -> Env -> String
peekVar var env = case result of
                    Just x -> show x
                    Nothing -> "Variable does not exist!"
               where result = lookup var env

performCommand :: Command -> Statement -> Run ()
performCommand N s = exec s
performCommand (Inspect var) s = do
    st <- get
    --putStr $ "Variable " ++ var ++ ": " 
    liftIO $ putStrLn $ peekVar var st--putStr $ show $ lookup var st
    presentMenu s

-- capitalise the first character in the input string because
-- it is annoying to have to use uppercase in command names
capitalise :: String -> String
capitalise (x:xs) = (toUpper x):xs
capitalise [] = []

presentMenu :: Statement -> Run ()
presentMenu s = do
    liftIO $ putStrLn "Input command: "
    input <- liftIO $ getLine
    let result = (readMaybe (capitalise input) :: Maybe Command)
    case result of
        Just cmd -> performCommand cmd s
        Nothing -> do 
            liftIO $ putStrLn "Invalid command!"
            presentMenu s
