import Data.Char (toLower, isDigit, isAlpha, isLower,isSpace)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)



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
    trace ("Push " ++ show n ++ "    Code: " ++ show code ++ "    Stack: " ++ stack2Str (IVal n : stack) ++ "    State: " ++ state2Str state) $
    run (code, IVal n : stack, state)
run (Add:code, IVal n1 : IVal n2 : stack, state) =
    trace ("AddCode: " ++ show code ++ "    Stack: " ++ stack2Str (IVal (n1 + n2) : stack) ++ "    State: " ++ state2Str state) $
    run (code, IVal (n1 + n2) : stack, state)
run (Sub:code, IVal n1 : IVal n2 : stack, state) =
    trace ("SubCode: " ++ show code ++ "    Stack: " ++ stack2Str (IVal (n1 - n2) : stack) ++ "    State: " ++ state2Str state) $
    run (code, IVal (n1 - n2) : stack, state)
run (Mult:code, IVal n1 : IVal n2 : stack, state) =
    trace ("MultCode: " ++ show code ++ "    Stack: " ++ stack2Str (IVal (n1 * n2) : stack) ++ "    State: " ++ state2Str state) $
    run (code, IVal (n1 * n2) : stack, state)
run (Tru:code, stack, state) =
    trace ("TruCode: " ++ show code ++ "    Stack: " ++ stack2Str (BVal True : stack) ++ "    State: " ++ state2Str state) $
    run (code, BVal True : stack, state)
run (Fals:code, stack, state) =
    trace ("FalsCode: " ++ show code ++ "    Stack: " ++ stack2Str (BVal False : stack) ++ "    State: " ++ state2Str state) $
    run (code, BVal False : stack, state)
run ((Store var):code, val:stack, (s, store)) =
    trace ("Store " ++ var ++ " " ++ showStackVal val ++ "     Pre-store: " ++ state2Str (s, store) ++ "    Code: " ++ show code ++     "Stack: " ++ stack2Str stack) $
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
        Just val -> trace ("Fetch " ++ varName ++ "    Code: " ++ show code ++ "    Stack: " ++ stack2Str (val : stack) ++ "    State: " ++ state2Str state) $ 
                    run (code, val : stack, state)
        Nothing  -> error "Run-time error"  -- Adjusted error message to match the requirement
run (Neg:code, BVal b : stack, state) =
    trace ("NegCode: " ++ show code ++ "    Stack: " ++ stack2Str (BVal (not b) : stack) ++ "    State: " ++ state2Str state) $
    run (code, BVal (not b) : stack, state)
run (Neg:code, stack, state) =
    error "Neg instruction expects a boolean value on top of the stack"
run (Equ:code, v1 : v2 : stack, state) =
    trace ("EquCode: " ++ show code ++ "    Stack: " ++ stack2Str (BVal (v1 == v2) : stack) ++ "    State: " ++ state2Str state) $
    run (code, BVal (v1 == v2) : stack, state)
run (Le:code, IVal n1 : IVal n2 : stack, state) =
    trace ("LeCode: " ++ show code ++ "    Stack: " ++ stack2Str (BVal (n1 <= n2) : stack) ++ "    State: " ++ state2Str state) $
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
    trace ("AndCode: " ++ show code ++ "  Stack: " ++ stack2Str (BVal (b1 && b2) : stack) ++ "    State: " ++ state2Str state) $
    run (code, BVal (b1 && b2) : stack, state)
run (And:_, _, _) =
    error "Runtime error: 'And' operation requires two boolean values on top of the stack"

-- Implement other instructions as needed.



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


sampleProgram :: String
sampleProgram = "x:=5;y:=4;z:=x+y;"

main :: IO ()
main = do
  let (instructions, finalState) = testParser sampleProgram
  putStrLn $ "Instructions: " ++ instructions
  putStrLn $ "Final State: " ++ finalState


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
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = let (token, rest) = span isAlpha (c:cs)
                in token : lexer rest
  | isDigit c = let (token, rest) = span isDigit (c:cs)
                in token : lexer rest
  | c == '=' && not (null cs) && head cs == '=' = ["=="] ++ lexer (drop 1 cs)
  | c == ':' && not (null cs) && head cs == '=' = [":="] ++ lexer (drop 1 cs)
  | c `elem` "+-*/:=();" = [c] : lexer cs
  | otherwise = lexer cs


parseStm :: [String] -> Either String ([Stm], [String])
parseStm [] = Right ([], [])
parseStm tokens = parseStm' tokens []

parseStm' :: [String] -> [Stm] -> Either String ([Stm], [String])
parseStm' [] stms = Right (reverse stms, [])
parseStm' tokens stms = do
  case tokens of
{-     ("if":rest) -> do

    ("while":rest) -> do -}

    (var : ":=" : rest) -> do
      (stm, rest') <- parseStmPart (var : ":=" : rest)
      case rest' of
        ";" : rest'' -> do
          (parsedStms, remainingTokens) <- parseStm' rest'' (stm : stms)
          Right (parsedStms, remainingTokens)
        _ -> Left $ "parseStm': expected semicolon after assignment, got " ++ show rest'
    _ -> Left $ "parseStm': unexpected tokens " ++ show tokens

parseStmPart :: [String] -> Either String (Stm, [String])
parseStmPart [] = Left "parseStmPart: unexpected end of input"
parseStmPart (var : ":=" : rest) = do
  (expr, rest') <- parseAexp rest
  case rest' of
    ";" : rest'' -> Right (SAssign var expr, rest'')
    _ -> Left $ "parseStmPart: expected semicolon after assignment, got " ++ show rest'
parseStmPart tokens = Left $ "parseStmPart: unexpected tokens " ++ show tokens


parseAexp :: [String] -> Either String (Aexp, [String])
parseAexp tokens = parseAddSub tokens

parseAddSub :: [String] -> Either String (Aexp, [String])
parseAddSub tokens = do
  (term1, rest) <- parseMulDiv tokens
  parseAddSub' rest term1

parseAddSub' :: [String] -> Aexp -> Either String (Aexp, [String])
parseAddSub' [] expr = Right (expr, [])
parseAddSub' (op : tokens) expr
  | op `elem` ["+", "-"] = do
    (term, rest) <- parseMulDiv tokens
    case op of
      "+" -> parseAddSub' rest (AAdd expr term)
      "-" -> parseAddSub' rest (ASub expr term)
      _   -> Left "Unexpected operator"
  | otherwise = Right (expr, op : tokens)

parseMulDiv :: [String] -> Either String (Aexp, [String])
parseMulDiv tokens = do
  (factor1, rest) <- parseTerm tokens
  parseMulDiv' rest factor1

parseMulDiv' :: [String] -> Aexp -> Either String (Aexp, [String])
parseMulDiv' [] expr = Right (expr, [])
parseMulDiv' (op : tokens) expr
  | op `elem` ["*", "/"] = do
    (factor, rest) <- parseTerm tokens
    case op of
      "*" -> parseMulDiv' rest (AMul expr factor)
      "/" -> parseMulDiv' rest (ADiv expr factor)
      _   -> Left "Unexpected operator"
  | otherwise = Right (expr, op : tokens)

parseTerm :: [String] -> Either String (Aexp, [String])
parseTerm [] = Left "parseTerm: unexpected end of input"
parseTerm (x:xs)
  | all isDigit x = Right (ALit (read x), xs)
  | isAlpha (head x) && isLower (head x) = Right (AVar x, xs)
  | x == "(" = do
    (expr, rest) <- parseAexp xs
    case rest of
      (")" : tokens) -> Right (expr, tokens)
      _ -> Left "parseTerm: missing closing parenthesis"
  | otherwise = Left $ "parseTerm: unexpected token " ++ show x


parse :: String -> [Stm]
parse str =
  let tokens = lexer str
      debugTokens = trace ("Tokens in parse: " ++ show tokens) tokens
  in unsafePerformIO $ do
       putStrLn $ "Tokens in parse: " ++ show tokens  -- Directly print tokens
       let parseUntilEmpty [] parsed = return parsed
           parseUntilEmpty remainingTokens parsed = do
             case parseStm remainingTokens of
               Left err -> error $ "Parsing error: " ++ err
               Right (stms, newRemaining) -> do
                 trace ("Parsed statements: " ++ show stms) (return ())
                 trace ("Remaining tokens: " ++ show newRemaining) (return ())
                 parseUntilEmpty newRemaining (parsed ++ stms)
       parseUntilEmpty tokens []


{- parse :: String -> [Stm]
parse str =
  let tokens = lexer str
      tokensStr = trace ("Tokens in parse: " ++ show tokens) tokens

  in case parseStm tokens of
       Left err -> error $ "Parsing error: " ++ err
       Right (stms, _) -> stms
 -}

 
{- testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

 -}


testParser :: String -> (String, String)
testParser programCode = (instructionStr, finalStateStr)
  where
    instructions = parse programCode
    instructionStr = trace ("Instructions generated from parsing: " ++ show instructions) ""
    (_, _, finalState) = run (compile instructions, createEmptyStack, createEmptyState)
    finalStateStr = state2Str finalState
