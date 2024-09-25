import Vm
import Data.Time.Clock
import Text.Printf

testModule :: Module
testModule = createModule
    [("FibRecur", 0, [
        [Clone 2, Const 3, Ge, JmpZ 2 1],
        [Clone 2, Const 1, Sub, Call "FibRecur", Cycle 2 1, Const 2, Sub, Call "FibRecur", Add, Ret],
        [Discard 1, Const 1, Ret]
    ])]

exceutePrint :: Module -> String -> Int -> IO ()
exceutePrint m n a = case runVM m n [a] of
    Just [v] -> printf "%s %d = %d\n" n a v
    _ -> printf "Error\n"

timeExec :: IO t -> IO t
timeExec a = do
    start <- getCurrentTime
    v <- a
    stop <- getCurrentTime
    putStr "\tTime: "
    print $ diffUTCTime stop start
    return v

test :: String -> Int -> IO ()
test n a = timeExec $ exceutePrint testModule n a

testFibRecur :: Int -> IO ()
testFibRecur = test "FibRecur"

main :: IO ()
main = do
    testFibRecur 5