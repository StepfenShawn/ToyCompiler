module Vm where

import Data.Array
import qualified Data.Map as Map

data Function = Function {
    functionName :: String,
    functionLocals :: Int,
    functionBlocks :: Array Int [Instruction]
} deriving (Show)

data Module = Module {
    moduleFuns :: Array Int Function,
    moduleMap :: Map.Map String Int
} deriving (Show)

-- instructions for Virtual Machine
data Instruction 
    = Const Int
    | Clone Int
    | Discard Int
    | Ctycle Int Int
    | Add | Sub | Mul | Div | Rem
    | Gt | Ge | Lt | Le | Eq | Ne | Not
    | CallI Int | Call String
    | Jmp Int | JmpZ Int Int | Ret
    | Err String
    | Save Int | Load Int
    deriving (Eq, Show, Ord)

data LocalStorage 
    = ArrayLS (Array Int Int)
    | EmptyLS
    deriving (Show)

data VM
    = VMError String
    | VMState [Int] [LocalStorage]
    deriving (Show)

-- create empty valid vm state
emptyVM :: VM
emptyVM = VMState [] []

-- push int-value to the stack and returns new vm state
pushInts :: [Int] -> VM -> VM
pushInts v (VMState s l) = VMState (v ++ s) l
pushInts _ vm = vm

-- pop int-value from the stack and returns new vm state
popInt :: VM -> (VM, Maybe Int)
popInt (VMState (x : xs) l) = (VMState xs l, Just x)
popInt (VMState _ _) = (VMError "Empty stack", Nothing)
popInt vm = (vm, Nothing)

-- exceute the instructions!
exceute :: Module -> VM -> [Instruction] -> (VM, Maybe Int)
exceute _ (VMError e) _ = (VMError e, Nothing)
exceute _ _ [] = (VMError "No more instructions", Nothing)
exceute module_ vm (x : xs) = case x of
    Const v -> f1 $ pushInts [v]
    Err e -> (VMError e, Nothing)
    where 
        f1 f = exceute module_ (f vm) xs