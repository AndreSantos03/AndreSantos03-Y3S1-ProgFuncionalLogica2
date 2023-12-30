import Data.Char (toLower, isDigit, isAlpha, isLower,isSpace)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)



data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | 
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





main :: IO ()
main = do
  let testBexp = parseBexpTokens ["(", "4", "+", "2", "<=", "x", "*", "y", ")"]
  case testBexp of
    Right (bexp, remainingTokens) ->
      putStrLn $ "Parsed Boolean expression: " ++ show bexp ++ ", Remaining Tokens: " ++ show remainingTokens
    Left err ->
      putStrLn $ "Error: " ++ err

data Aexp = ALit Integer
          | AVar String
          | AAdd Aexp Aexp
          | ASub Aexp Aexp
          | AMul Aexp Aexp
          | ADiv Aexp Aexp
          | ATrue
          | AFalse
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
         | Noop
         deriving Show

compileAexp :: Aexp -> Code
compileAexp (ALit n) = [Push n]
compileAexp (AVar x) = [Fetch x]
compileAexp (AAdd a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Add]
compileAexp (ASub a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Sub]
compileAexp (AMul a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Mult]
compileAexp (ATrue) = [Tru]
compileAexp (AFalse) = [Fals]

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


parseStm :: [String] -> Either String (Stm, [String])
parseStm [] = Right (Noop, [])
  -- Assuming Noop is a valid no-operation statement
parseStm tokens = parseStm' tokens []

parseStm' :: [String] -> [Stm] -> Either String (Stm, [String])
parseStm' [] stms = Right (foldr1 SSeq (reverse stms), [])
parseStm' tokens stms = do
  (stm, remainingTokens) <- parseStmPart tokens
  case remainingTokens of
    ";" : rest -> parseStm' rest (stm : stms)
    _ -> Left $ "parseStm': expected semicolon after assignment, got " ++ show remainingTokens

-- SSeq should be a statement that represents a sequence of statements

parseStmPart :: [String] -> Either String (Stm, [String])
parseStmPart [] = Left "parseStmPart: unexpected end of input"
parseStmPart ("if" : rest) = parseIf ("if" : rest)
parseStmPart (var : ":=" : rest) = do
  (expr, rest') <- parseAexp rest
  Right (SAssign var expr, rest')
-- ... other cases such as "while", "sequence of statements", etc.


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
parseTerm ("(":rest) = do
  (exp, restTokens) <- parseAexp rest
  case restTokens of
    ")":moreTokens -> Right (exp, moreTokens)
    _ -> Left "parseTerm: missing closing parenthesis"
parseTerm ("true":xs) = Right (ATrue, xs)
parseTerm ("false":xs) = Right (AFalse, xs)
parseTerm (x:xs)
  | all isDigit x = Right (ALit (read x), xs)
  | isAlpha (head x) && isLower (head x) = Right (AVar x, xs)
  | otherwise = Left $ "parseTerm: unexpected token " ++ show x

-- This function is served to pick the code inside the conditionals and the then and else statements
extractInsideCode :: [String] -> ([String], [String])
extractInsideCode tokens = go [] tokens
  where
    go acc ("if":xs) = takeUntil "then" xs
    go acc ("then":xs) = takeUntil "else" xs
    go acc ("else":xs) = (xs,[])
    go acc ("(":xs) = go ("(":acc) xs
    go acc (")":xs) =
      if null acc then ([], xs)
      else let (parenthesized, rest) = span (/= "(") acc
           in go parenthesized xs
    go acc (x:xs) = go (x : acc) xs
    go acc [] = (reverse acc, [])
    
    takeUntil :: String -> [String] -> ([String], [String])
    takeUntil delim tokens =
      let (beforeDelim, afterDelim) = break (== delim) tokens
      in (filter (not . null) beforeDelim, afterDelim)

--Must pass to it with the token starting with if
parseIf :: [String] -> Either String (Stm, [String])
parseIf tokens = do
    let (conditionTokens, rest1) = extractInsideCode tokens
    trace ("Conditional tokens: " ++ show conditionTokens) $ return ()
    (condition, rest2) <- parseBexpTokens conditionTokens
    let (thenTokens, rest3) = extractInsideCode rest1
    trace ("Then tokens: " ++ show thenTokens) $ return ()
    (thenStatement, rest4) <- parseStm thenTokens
    let (elseTokens, rest5) = extractInsideCode rest3
    trace ("Else tokens: " ++ show elseTokens) $ return ()
    (elseStatement, rest6) <- parseStm elseTokens
    Right (SIf condition thenStatement elseStatement, rest5)

parseNegatedBexp :: [String] -> Either String (Bexp, [String])
parseNegatedBexp ("(":rest) = do
  (bexp, remaining) <- parseBexpTokens rest
  Right (BNot bexp, remaining)
parseNegatedBexp rest = do
  (bexp, remaining) <- parseBexpTokens rest
  Right (BNot bexp, remaining)


    
parseBexpTokens :: [String] -> Either String (Bexp, [String])

parseBexpTokens ("(":rest) = do
  (bexp, remaining) <- parseBexpTokens rest
  case remaining of
    [] -> Left "parseBexpTokens: Missing closing parenthesis"
    (")":xs) -> Right (bexp, xs)
    _ -> Left "parseBexpTokens: Invalid expression or comparison"
parseBexpTokens ("not":rest) = do
  (bexp, remaining) <- parseBexpTokens rest
  Right (BNot bexp, remaining)
parseBexpTokens (x:"==":xs) = do
  (a1, remaining1) <- parseAexp [x]
  (a2, remaining2) <- parseAexp xs
  Right (BEq a1 a2, remaining2)
parseBexpTokens (x:"<=":xs) = do
  (a1, remaining1) <- parseAexp [x]
  (a2, remaining2) <- parseAexp xs
  Right (BLe a1 a2, remaining2)
parseBexpTokens _ = Left "parseBexpTokens: Invalid expression or comparison"



parse :: String -> [Stm]
parse str =
  let tokens = lexer str
  in unsafePerformIO $ do
       putStrLn $ "Tokens in parse: " ++ show tokens
       parseUntilEmpty tokens []

  where
    parseUntilEmpty :: [String] -> [Stm] -> IO [Stm]
    parseUntilEmpty [] parsed = return parsed
    parseUntilEmpty remainingTokens parsed = do
      case parseStm remainingTokens of
        Left err -> error $ "Parsing error: " ++ err
        Right (stm, newRemaining) -> parseUntilEmpty newRemaining (parsed ++ [stm])



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
