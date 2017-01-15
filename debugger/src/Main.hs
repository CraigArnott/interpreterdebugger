{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

import qualified System.IO as System

import Text.Read (readMaybe) 
import Data.Char (toUpper)
import Data.List (nub, (\\))

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- Our mainline                                                    -}
{-------------------------------------------------------------------}
 
main = do
    putStrLn "Input file name:"
    file <- getLine
    runInterpreter file

runInterpreter :: String -> IO ()
runInterpreter file = do
    contents <- readFile file
    let program = (read contents :: Statement)
    runCheck program
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
type Env = [Map.Map Name Val]
type Trace = [Statement] -- type representing the statements executed to get to the current point
type Stage = (Trace, Env) -- type representing a step in the interpretation process

lookup k t = case Map.lookup k t of
    Just x -> return x
    Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator, 
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Stage (ExceptT String Identity) a 

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
    lookup s $ head $ snd env

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
    result <- runExceptT $ (runStateT $ exec s) ([], [Map.empty])
    case result of
        Right _ -> return ()
        Left e -> System.print ("Uncaught exception: " ++ e)

type Run a = StateT Stage (ExceptT String IO) a 
runRun p =  runExceptT (runStateT p Map.empty) 

-- reset to the previous state
goBack :: Run ()
goBack = state $ (\(x:xs, y:ys) -> ((), (xs,ys)))

-- set the previous statement as the given statement
setStat :: Statement -> Run ()
setStat stat = state $ (\(stats, tables) -> ((), (stat:stats, tables))) 

-- set the given variable to the given value and create a new mapping table containing the new mapping
set :: (Name, Val) -> Run ()
set (s,i) = state $ (\(stats,tables) -> ((), (stats, (Map.insert s i $ head tables) : tables)))

dupHead :: [a] -> [a]
dupHead [] = []
dupHead (x:xs) = x:x:xs

-- simply duplicate the environment when making no changes to variables
setNone :: Run ()
setNone = state $ (\(stats, tables) -> ((), (stats, dupHead tables)))

exec :: Statement -> Run ()

exec (Assign s v) = do
    st <- get  
    let env = snd st
    Right val <- return $ runEval st (eval v)  
    set (s,val)
    setStat (Assign s v)

-- don't record any state info for Seq statements, this makes the debugging process cleaner
exec (Seq s0 s1) = do presentMenu s0 >> presentMenu s1 

exec (Print e) = do 
    st <- get
    Right val <- return $ runEval st (eval e) 
    liftIO $ System.print val
    setStat (Print e)
    setNone

exec (If cond s0 s1) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    setStat (If cond s0 s1)
    setNone
    if val then do presentMenu s0 else do presentMenu s1

-- we "unroll" loops by not saving state information
-- this is because the comparison will always yield the same result
-- makes debugging cleaner
exec (While cond s) = do
    st <- get
    Right (B val) <- return $ runEval st (eval cond)
    if val then do presentMenu s >> presentMenu (While cond s) else return ()

exec (Try s0 s1) = do catchError (presentMenu s0) (\e -> presentMenu s1)

exec Pass = return ()

{-------------------------------------------------------------------}
{- State inspection operations                                     -}
{-------------------------------------------------------------------}

peekVar :: String -> Env -> String
peekVar var env = case result of
                    Just x -> show x ++ "\n"
                    Nothing -> "Variable does not exist (yet)!\n"
               where result = lookup var $ head env

listVars :: Env -> String
listVars env = show $ Map.keys $ head env

history :: String -> Env -> String
history _ [] = "Beginning of history...\n"
history var (env:rest) = (history var rest) ++ (peekVar var (env:rest))

pastOps :: Trace -> String
pastOps [] = []
pastOps (t:ts) = (pastOps ts) ++ "\n" ++ (show t)

{-------------------------------------------------------------------}
{- Logic for the options menu                                      -}
{-------------------------------------------------------------------}

data Command = N -- execute next instruction
             | P -- go back to previous state 
             | O -- options
             | Vars -- display all variables that currently hold values
             | Past -- display all previously executed statements
             | Inspect String -- view the current state of the given  variable
             | History String -- view the history of the given variable from the beginning of execution
    deriving (Show, Read, Eq)

-- handle all the commands outlined above
performCommand :: Command -> Statement -> Run ()

performCommand N s = exec s

performCommand P s = do
    st <- get
    goBack
    presentMenu $ head $ fst st
    presentMenu s

performCommand O s = do
    liftIO $ putStrLn optionsString
    presentMenu s

performCommand Vars s = do
    st <- get
    liftIO $ putStrLn $ listVars $ snd st
    presentMenu s

performCommand Past s = do
    st <- get
    liftIO $ putStrLn $ pastOps $ fst st
    presentMenu s

performCommand (Inspect var) s = do
    st <- get
    liftIO $ putStrLn $ peekVar var $ snd st
    presentMenu s

performCommand (History var) s = do
    st <- get
    let env = snd st
    liftIO $ putStrLn $ (history var env) ++ "End of history!"
    presentMenu s

-- Capitalise the first character in the input string
-- Using uppercase on the command line is annoying!
capitalise :: String -> String
capitalise (x:xs) = (toUpper x):xs
capitalise [] = []

presentMenu :: Statement -> Run () 

-- no need to present choice for Seq actions, it just annoys the user
presentMenu (Seq a b) = exec $ Seq a b 

-- for all other actions, give the user a prompt asking them what to do
presentMenu s = do
    liftIO $ putStrLn $ "Current statement: " ++ show s
    liftIO $ putStrLn "Input command (o for options): "
    input <- liftIO $ getLine
    let result = (readMaybe (capitalise input) :: Maybe Command)
    case result of
        Just cmd -> performCommand cmd s
        Nothing -> do 
            liftIO $ putStrLn "Invalid command!"
            presentMenu s

-- Our options prompt
optionsString = "Options:\nn: execute next command\np: go back to previous state\npast: view a list of previously executed statements\nvars: view the list of variables that currently have values\ninspect \"<varname>\": inspect a variable's current state\nhistory \"<varname>\": inspect the history of a variable"

{-------------------------------------------------------------------}
{- Pre-run analysis functions                                      -}
{-------------------------------------------------------------------}

-- check to see if a program has any variables that are written to but never used
runCheck :: Statement -> IO ()
runCheck s = do
    let unused = checkUnused s
    case unused of
        [] -> putStrLn "Check completed, no unused variables found!"
        x -> putStrLn $ "Unused variables detected: " ++ (show x)

checkUnused :: Statement -> [String]
checkUnused s = writtenTo \\ readFrom
        where readFrom = nub $ getReadFrom s
              writtenTo = nub $ getWrittenTo s

-- get a list of all the variables that are read from in a program
getReadFrom :: Statement -> [String]
getReadFrom (Assign _ e) = getReadFromExpr e
getReadFrom (Print e) = getReadFromExpr e
getReadFrom (If cond l r) = getReadFromExpr cond ++ getReadFrom l ++ getReadFrom r 
getReadFrom (While cond s) = getReadFromExpr cond ++ getReadFrom s
getReadFrom (Seq l r) = getReadFrom l ++ getReadFrom r
getReadFrom (Try l r) = getReadFrom l ++ getReadFrom r

-- same as above but for expressions
getReadFromExpr :: Expr -> [String]
getReadFromExpr (Const _) = []
getReadFromExpr (Var x) = [x]
getReadFromExpr (Not x) = getReadFromExpr x
getReadFromExpr (Mul l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Div l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Add l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Sub l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (And l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Or l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Eq l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Gt l r) = getReadFromExpr l ++ getReadFromExpr r
getReadFromExpr (Lt l r) = getReadFromExpr l ++ getReadFromExpr r

-- get a list of all the variables that are written to in a program
getWrittenTo :: Statement -> [String]
getWrittenTo (Print _) = []
getWrittenTo (Assign x _) = [x]
getWrittenTo (While _ x) = getWrittenTo x
getWrittenTo (Seq l r) = getWrittenTo l ++ getWrittenTo r
getWrittenTo (If _ l r) = getWrittenTo l ++ getWrittenTo r
getWrittenTo (Try l r) = getWrittenTo l ++ getWrittenTo r
