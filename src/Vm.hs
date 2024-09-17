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
    | Cycle Int Int
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

-- pop local storage and returns new vm state
popLocal :: VM -> VM
popLocal (VMState s (_ : ls)) = VMState s ls
popLocal (VMState _ _) = VMError "No local storage to pop"
popLocal vm = vm

-- push local storage and returns new vm state
pushLocal :: Int -> VM -> VM
pushLocal 0 (VMState s l) = VMState s $ EmptyLS : l
pushLocal n (VMState s l) = VMState s $ ArrayLS (listArray (0, n - 1) [0..]) : l
pushLocal _ vm = vm

-- push int-value to the stack and returns new vm state
pushInts :: [Int] -> VM -> VM
pushInts v (VMState s l) = VMState (v ++ s) l
pushInts _ vm = vm

-- pop int-value from the stack and returns new vm state
popInt :: VM -> (VM, Maybe Int)
popInt (VMState (x : xs) l) = (VMState xs l, Just x)
popInt (VMState _ _) = (VMError "Empty stack", Nothing)
popInt vm = (vm, Nothing)

cloneTop :: Int -> VM -> VM
cloneTop n (VMState (x : xs) l) = VMState (take n (repeat x) ++ xs) l 
cloneTop _ (VMState _ _) = VMError "Empty stack"
cloneTop _ vm = vm

cycleTop :: Int -> Int -> VM -> VM
cycleTop n c (VMState s l) = VMState ((cycleList c $ take n s) ++ drop n s) l
    where
        cycleList n' l' = drop n' l' ++ take n' l'
cycleTop _ _ vm = vm

binOp :: (Int -> Int -> Int) -> VM -> VM
binOp f (VMState (x1:x2:xs) l) = VMState (x2 `f` x1 : xs) l
binOp _ (VMState _ _) = VMError "Not enough arguments"
binOp _ vm = vm

-- exceute the instructions!
exceute :: Module -> VM -> [Instruction] -> (VM, Maybe Int)
exceute _ (VMError e) _ = (VMError e, Nothing)
exceute _ _ [] = (VMError "No more instructions", Nothing)
exceute module_ vm (x : xs) = case x of
    Const v -> f1 $ pushInts [v]
    Err e -> (VMError e, Nothing)
    where 
        f1 f = exceute module_ (f vm) xs

-- get Function on index from Module
getFunction :: Module -> Int -> Function
getFunction m i = (moduleFuns m) ! i

-- lookup function index by name
lookupFunction :: Module -> String -> Maybe Int
lookupFunction m n = Map.lookup n $ moduleMap m

-- execute function on index from Module
exceuteFunction :: Module -> Int -> VM -> VM
exceuteFunction m fi v = popLocal $ go (pushLocal fl v) 0
    where
        fn = getFunction m fi
        fl = functionLocals fn
        go vm bi = case exceute m vm $ functionBlocks fn ! bi of
            (vm', Just bi') -> go vm' bi'
            (vm', Nothing) -> vm'

-- create module from functions
createModule :: [(String, Int, [[Instruction]])] -> Module
createModule f = Module mf mm
    where
        -- module function name-index map
        mm = Map.fromList $ zip (map (\(fn, _, _) -> fn) f) [0..]
        -- module functions
        mf = listArray (0, length f - 1) $ map (constructTransform) f
        -- construct function
        construct n l b = Function n l $ listArray (0, length b - 1) b
        -- transform function
        constructTransform (fn, fl, fi) = construct fn f1 $ map (map swapCall) fi
        -- swap named calls for indexed calls
        swapCall (Call cn) = case Map.lookup cn $ mm of
            Just i -> CallI i
            Nothing -> Err "Unknown function"
        swapCall c = c

-- run function on empty VM with predefined stack
runVM :: Module -> String -> [Int] -> Maybe [Int]
runVM m n a = case lookupFunction m n of
    Just i -> case exceuteFunction m i (pushInts a emptyVM) of
        VMState s _ -> Just s
        _ -> Nothing
    _ -> Nothing