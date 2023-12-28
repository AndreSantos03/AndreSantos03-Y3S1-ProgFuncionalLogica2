import Data.Char (toLower, isDigit, isAlpha, isLower)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)


data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackVal = IVal Integer | BVal Bool | TVal String deriving (Show, Eq)
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
stack2Str stack = intercalate "," (map showStackVal stack)


showStackVal :: StackVal -> String
showStackVal (IVal n) = show n
showStackVal (BVal True) = "True"
showStackVal (BVal False) = "False"

state2Str :: State -> String
state2Str (_, store) =
    intercalate "," . map showVarVal . sortBy (comparing fst) $ store
  where
    showVarVal :: (String, StackVal) -> String
    showVarVal (var, IVal n) = var ++ "=" ++ show n
    showVarVal (var, BVal True) = var ++ "=True"
    showVarVal (var, BVal False) = var ++ "=False"
    showVarVal (var, TVal s) = var ++ "=" ++ s  -- This line can actually be removed if you're not using TVal anymore.

evaluateCondition :: Code -> State -> Bool
evaluateCondition condition state = 
  let (_, stack, _) = run (condition, [], state) -- You might need to adjust this call according to how 'run' is defined
  in case stack of
       (BVal b : _) -> not b -- Assuming the condition leaves a boolean on top of the stack indicating whether to exit the loop
       _ -> error "Condition code did not leave a boolean value on the stack"

-- Assuming the top of the stack is a BVal containing the condition result
getConditionResult :: Stack -> Bool
getConditionResult (BVal result : _) = result
getConditionResult _ = error "Condition did not evaluate to a boolean value"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) =
    -- trace ("Push " ++ show n) $
    trace ("Push " ++ show n ++ " | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, IVal n : stack, state)
run (Add:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Add | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, IVal (n1 + n2) : stack, state)
run (Sub:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Sub | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, IVal (n1 - n2) : stack, state)
run (Mult:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Mult | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, IVal (n1 * n2) : stack, state)
run (Tru:code, stack, state) =
    trace ("Tru | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, BVal True : stack, state)
run (Fals:code, stack, state) =
    trace ("Fals | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack ++ " | State: " ++ state2Str state) $
    run (code, BVal False : stack, state)
run ((Store var):code, val:stack, (s, store)) =
    trace ("Store " ++ var ++ " " ++ showStackVal val ++ " Pre-store: " ++ state2Str (s, store) ++ " | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $
    let updatedStore = updateStore var val store
    in trace ("Post-store: " ++ state2Str (s, updatedStore)) $
       run (code, stack, (s, updatedStore))
  where
    updateStore var val [] = [(var, val)]
    updateStore var val ((v,sVal):vs)
      | v == var  = (var, val) : vs
      | otherwise = (v, sVal) : updateStore var val vs
run ((Fetch varName):code, stack, state@(_, store)) =
    case lookup varName store of
        Just val -> trace ("Fetch " ++ varName ++ " | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $ run (code, val : stack, state)
        Nothing  -> error "Run-time error"  -- Adjusted error message to match the requirement
run (Neg:code, BVal b : stack, state) =
    trace ("Neg | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $
    run (code, BVal (not b) : stack, state)
run (Neg:code, stack, state) =
    error "Neg instruction expects a boolean value on top of the stack"
run (Equ:code, v1 : v2 : stack, state) =
    trace ("Equ | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $
    run (code, BVal (v1 == v2) : stack, state)
run (Le:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Le | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $
    run (code, BVal (n1 <= n2) : stack, state)  -- Ensure that n1 is the last pushed value
run (Le:_, _, _) =
    error "Le instruction requires two integer values on top of the stack"
run (Loop condition body:restCode, stack, state) = 
    let (_, conditionStack, _) = run (condition, stack, state)
    in if getConditionResult conditionStack
       then let (_, bodyStack, bodyState) = run (body, stack, state)
            in run (Loop condition body:restCode, bodyStack, bodyState)
       else run (restCode, stack, state)
run (And:code, BVal b1 : BVal b2 : stack, state) =
    trace ("And | Code: " ++ show code ++ " | Stack: " ++ stack2Str stack) $
    run (code, BVal (b1 && b2) : stack, state)
run (And:_, _, _) =
    error "Run-time error: 'And' operation requires two boolean values on top of the stack"

-- Implement other instructions as needed.

-- Tests
testExample1 :: IO ()
testExample1 = do
  let initialState = ([], [("x", IVal 3)]) 
      program = [Push (-1), Fetch "x", Add, Store "x"] 
      expectedState = ([], [("x", IVal 4)])

  let (_, _, finalState) = run (program, [], initialState)

  if finalState == expectedState
    then putStrLn "Test Example 1 was passed"
    else putStrLn "Test Example 1 failed"



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
    let input1 = ["5"]
        input2 = ["x"]
        input3 = ["(", "5", "+", "3", ")"]
        input4 = ["(", "x", "+", "y", ")"]
        input5 = ["(", "5", "+", "3"]

    putStrLn "Testing parseAexp:"
    putStrLn "-------------------"

    putStrLn "Input 1:"
    print (parseAexp input1)

    putStrLn "Input 2:"
    print (parseAexp input2)

    putStrLn "Input 3:"
    print (parseAexp input3)

    putStrLn "Input 4:"
    print (parseAexp input4)

    putStrLn "Input 5:"
    print (parseAexp input5)



data Aexp = ALit Integer
          | AVar String
          | AAdd Aexp Aexp
          | ASub Aexp Aexp
          | AMul Aexp Aexp
          | ADiv Aexp Aexp
          deriving Show

data Bexp = BLit Bool
          | BEq Aexp Aexp
          | BLe Aexp Aexp
          | BAnd Bexp Bexp
          | BOr Bexp Bexp
          | BNot Bexp
          deriving Show

data Stm = SAssign String Aexp
         | SSeq Stm Stm
         | SIf Bexp Stm Stm
         | SWhile Bexp Stm
         deriving Show

compileAexp :: Aexp -> Code
compileAexp (ALit n) = [Push n]
compileAexp (AVar x) = [Fetch x]
compileAexp (AAdd a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Add]
compileAexp (ASub a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Sub]
compileAexp (AMul a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Mult]

compileBexp :: Bexp -> Code
compileBexp (BLit b) = [if b then Tru else Fals]
compileBexp (BEq a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Equ]
compileBexp (BLe a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Le]
compileBexp (BAnd b1 b2) = compileBexp b1 ++ compileBexp b2 ++ [And]
compileBexp (BNot b) = compileBexp b ++ [Neg]

-- Compiles a single statement into Code
compileStm :: Stm -> Code
compileStm (SAssign x a) = compileAexp a ++ [Store x]
compileStm (SSeq s1 s2) = compileStm s1 ++ compileStm s2
compileStm (SIf b s1 s2) = compileBexp b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (SWhile b s) = [Loop (compileBexp b) (compileStm s)]

-- Compiles a list of statements into Code
compile :: [Stm] -> Code
compile statements = concatMap compileStm statements

lexer :: String -> [String]
lexer = words . map (\c -> if c `elem` ";()" then ' ' else c)

{- -- Quick Lexer test
testLexer :: IO ()
testLexer = do
    let testCases = [
            "23 + 4 * 421",
            "(x + y) * 5",
            "if (x < 10) then y else 0",
            "x := 5; y := x + 10"
            ]
    putStrLn "Testing lexer..."
    mapM_ (\expr -> putStrLn $ "Expression: " ++ expr ++ " Tokens: " ++ show (lexer expr)) testCases
 -}


-- Parses a list of statements from a list of tokens.
parseStms :: [String] -> ([Stm], [String])
parseStms [] = ([], [])
parseStms tokens =
  let (stm, rest) = parseStm tokens
      (stms, rest') = parseStms rest
  in (stm : stms, rest')  -- Recursively build the list of statements



{- parseStm :: [String] -> (Stm, [String])
parseStm tokens = 
  let debugTokens = show tokens
      debugResult = case tokens of
        (var : ":=" : rest) ->
          case parseAexp rest of
            Left errMsg -> error errMsg  -- Handle parsing error here
            Right (expr, rest') ->
              case rest' of 
                ";" : rest'' -> (SAssign var expr, rest'')
                _ -> (SAssign var expr, rest')
        _ -> error $ "parseStm: unexpected tokens: " ++ show tokens
  in trace ("parseStm called with tokens: " ++ debugTokens ++ " and produced: " ++ show debugResult) debugResult


-- Parses an arithmetic expression
parseAexp :: [String] -> Either String (Aexp, [String])
parseAexp [] = Left "parseAexp: unexpected end of input"
parseAexp (x:xs)
  | all isDigit x = Right (ALit (read x), xs)
  | isAlpha (head x) && isLower (head x) = Right (AVar x, xs)
  | x == "(" =
      case parseAexp xs of
        Left errMsg -> Left errMsg
        Right (a1, op:rest2) ->
          case parseAexp rest2 of
            Left errMsg -> Left errMsg
            Right (a2, ")" : rest3) ->
              case op of
                "+" -> Right (AAdd a1 a2, rest3)
                "-" -> Right (ASub a1 a2, rest3)
                "*" -> Right (AMul a1 a2, rest3)
                _   -> Left $ "parseAexp: unknown operator " ++ op
            _ -> Left "parseAexp: missing closing parenthesis"
        _ -> Left "parseAexp: missing left operand"
  | otherwise = Left $ "parseAexp: unexpected token " ++ show x
 -}

parseStm :: [String] -> (Stm, [String])
parseStm tokens =
  let debugTokens = show tokens
      debugResult = case tokens of
        (var : ":=" : rest) ->
          case parseAexp rest of
            Left errMsg -> error errMsg
            Right (expr, ";" : rest') -> (SAssign var expr, rest')
            _ -> error $ "parseStm: expected semicolon after assignment, got " ++ show rest'
        _ -> error $ "parseStm: unexpected tokens: " ++ show tokens
  in trace ("parseStm called with tokens: " ++ debugTokens ++ " and produced: " ++ show debugResult) debugResult

parseAexp :: [String] -> Either String (Aexp, [String])
parseAexp [] = Left "parseAexp: unexpected end of input"
parseAexp (x:xs)
  | all isDigit x = Right (ALit (read x), xs)
  | isAlpha (head x) && isLower (head x) = Right (AVar x, xs)
  | x == "(" =
      case parseAexp xs of
        Left errMsg -> Left errMsg
        Right (a1, op:rest2) ->
          case parseAexp rest2 of
            Left errMsg -> Left errMsg
            Right (a2, ")" : rest3) ->
              case op of
                "+" -> Right (AAdd a1 a2, rest3)
                "-" -> Right (ASub a1 a2, rest3)
                "*" -> Right (AMul a1 a2, rest3)
                _   -> Left $ "parseAexp: unknown operator " ++ op
            _ -> Left "parseAexp: missing closing parenthesis"
        _ -> Left "parseAexp: missing left operand"
  | otherwise = Left $ "parseAexp: unexpected token " ++ show x

testParseAexp :: IO ()
testParseAexp = do
    putStrLn "Testing parseAexp..."
    let testCases = [
            ["5"], -- ALit
            ["x"], -- AVar
            ["(", "5", "+", "3", ")"], -- AAdd
            ["(", "x", "-", "y", ")"], -- ASub
            ["7", "*", "4"], -- AMul
            ["(","7", "*", "4", ")"] -- AMul
                    ]
    mapM_ (\testCase -> putStrLn $ "Input: " ++ show testCase ++ " Result: " ++ show (parseAexp testCase)) testCases

-- Parses the entire program string into a list of statements
parse :: String -> [Stm]
parse str = 
  let tokens = lexer str
      (stms, _) = parseStms tokens
  in stms


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

