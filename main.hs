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
showStackVal (BVal b) = show b
showStackVal (TVal s) = s



state2Str :: State -> String
state2Str (_, store) =
    intercalate "," . map showVarVal . sortBy (comparing fst) $ store
  where
    showVarVal :: (String, StackVal) -> String
    showVarVal (var, IVal n) = var ++ "=" ++ show n
    showVarVal (var, BVal True) = var ++ "=True"
    showVarVal (var, BVal False) = var ++ "=False"
    showVarVal (var, TVal s) = var ++ "=" ++ s  -- This line can actually be removed if you're not using TVal anymore.

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
    run (code, BVal True : stack, state)
run (Fals:code, stack, state) =
    trace ("Fals") $
    run (code, BVal False : stack, state)
run ((Store var):code, val:stack, (s, store)) =
    trace ("Store " ++ var) $
    run (code, stack, (s, (var, val) : store))
-- Implement other instructions as needed.



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
  let testResult = testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]
  let (stackStr, stateStr) = testResult -- Unpack the tuple into stackStr and stateStr
  putStrLn "Stack:"
  putStrLn stackStr -- Corrected to use stackStr
  putStrLn "State:"
  putStrLn stateStr -- Corrected to use stateStr
  let expectedResult = ("", "a=3,someVar=False,var=True")
  print (testResult == expectedResult) -- This compares the entire tuple
  -- The following lines are likely not necessary if the above print statement is what you want
  -- print (stackStr == fst expectedResult) -- Compare stack string to expected stack string
  -- print (stateStr == snd expectedResult) -- Compare state string to expected state string
  -- If you still want to print stackStr and stateStr separately, you can uncomment these lines:
  -- print stackStr  -- Print the actual stack string
  -- print stateStr  -- Print the actual state string
