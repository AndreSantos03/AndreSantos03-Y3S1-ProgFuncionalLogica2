import Data.Char (toLower, isDigit, isAlpha, isLower,isSpace)
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
    trace ("Store " ++ var ++ " " ++ showStackVal val ++ " Pre-store: " ++ state2Str (s, store)) $
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
        Just val -> trace ("Fetch " ++ varName) $ run (code, val : stack, state)
        Nothing  -> error "Run-time error"  -- Adjusted error message to match the requirement
run (Neg:code, BVal b : stack, state) =
    trace ("Neg") $
    run (code, BVal (not b) : stack, state)
run (Neg:code, stack, state) =
    error "Neg instruction expects a boolean value on top of the stack"
run (Equ:code, v1 : v2 : stack, state) =
    trace ("Equ") $
    run (code, BVal (v1 == v2) : stack, state)
run (Le:code, IVal n1 : IVal n2 : stack, state) =
    trace ("Le") $
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
    trace ("And") $
    run (code, BVal (b1 && b2) : stack, state)
run (And:_, _, _) =
    error "Run-time error: 'And' operation requires two boolean values on top of the stack"

-- Implement other instructions as needed.



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


main :: IO ()
main = do
    let input1 = "x := 5; y := (3 + 2);"
        input2 = "if (x == y) then (x := 1) else (x := 0);"
        input3 = "while (x <= 10) do (x := (x + 1));"
        
    putStrLn "Testing lexer:"
    putStrLn "----------------"

    putStrLn "Input 1:"
    print (lexer input1)

    putStrLn "Input 2:"
    print (lexer input2)

    putStrLn "Input 3:"
    print (lexer input3)

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
  | c `elem` "(){}[]:;" = [c] : lexer cs
  | isSpace c = lexer (dropWhile isSpace cs)
  | otherwise = let (token, rest) = break (\x -> isSpace x || x `elem` "(){}[]:;") (c:cs)
                in token : lexer rest


-- Parses a list of statements from a list of tokens.
parseStms :: [String] -> Either String ([Stm], [String])
parseStms [] = Right ([], [])
parseStms tokens = parseStms' tokens []

parseStms' :: [String] -> [Stm] -> Either String ([Stm], [String])
parseStms' [] stms = Right (reverse stms, [])
parseStms' tokens stms = do
  (stm, rest) <- parseStm tokens
  parseStms' rest (stm : stms)

parseStm :: [String] -> Either String (Stm, [String])
parseStm [] = Left "parseStm: unexpected end of input"
parseStm (var : ":=" : rest) = do
  (expr, rest') <- parseAexp rest
  case rest' of
    ";" : rest'' -> Right (SAssign var expr, rest'')
    _ -> Left $ "parseStm: expected semicolon after assignment, got " ++ show rest'
parseStm _ = Left "parseStm: unexpected tokens"

-- Parses an arithmetic expression
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
  in case parseStms tokens of
       Left err -> error $ "Parsing error: " ++ err
       Right (stms, _) -> stms

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

testParseFunction :: String -> IO ()
testParseFunction programCode = do
  putStrLn "Testing parse function:"
  let parsedStatements = parse programCode
  putStrLn "Parsed statements:"
  print parsedStatements

