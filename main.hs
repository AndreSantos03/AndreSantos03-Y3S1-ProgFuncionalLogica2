import Data.Char (toLower)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackVal = IVal Integer | BVal Bool | TVal String deriving Show
type Stack = [StackVal]

tt :: StackVal
tt = TVal "tt"

ff :: StackVal
ff = TVal "ff"

type State = (Stack, [(String, StackVal)])

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = (createEmptyStack, [])

stack2Str :: Stack -> String
stack2Str = intercalate "," . reverse . map showStackVal

showStackVal :: StackVal -> String
showStackVal (IVal n) = show n
showStackVal (BVal True) = "tt"
showStackVal (BVal False) = "ff"
showStackVal (TVal s) = s


state2Str :: State -> String
state2Str (_, store) =
    intercalate "," . map showVarVal . sortBy (comparing fst) $ store
  where
    showVarVal :: (String, StackVal) -> String
    showVarVal (var, IVal n) = var ++ "=" ++ show n
    showVarVal (var, BVal True) = var ++ "=tt"
    showVarVal (var, BVal False) = var ++ "=ff"
    showVarVal (var, TVal s) = var ++ "=" ++ s


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) =
    trace ("Push " ++ show n) $
    run (code, IVal n : stack, state)
run (Add:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Add") $
    run (code, IVal (n1 + n2) : stack, state)
run (Sub:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Sub") $
    run (code, IVal (n1 - n2) : stack, state)
run (Mult:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Mult") $
    run (code, IVal (n1 * n2) : stack, state)
run (Tru:code, stack, state) =
    trace ("Tru") $
    run (code, tt : stack, state)
run (Fals:code, stack, state) =
    trace ("Fals") $
    run (code, ff : stack, state)
run ((Store var):code, val:stack, (s, store)) =
    trace ("Store " ++ var) $
    run (code, stack, (s, (var, val) : store))
-- Implement other instructions as needed.



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
  let (stackStr, stateStr) = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] 
  putStrLn "Stack:"
  putStrLn stackStr
  putStrLn "State:"
  putStrLn stateStr
  let expectedResult = ("-10","")
  print (stackStr == (fst expectedResult))  -- Compare stack string to expected stack string
  print (stateStr == (snd expectedResult))  -- Compare state string to expected state string
  print stackStr  -- Print the actual stack string
  print stateStr  -- Print the actual state string

