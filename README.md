# TP2 Coursework-Haskell

T03_G07

André Bernardo Ferreira Santos (up202108658)

Inês Ferreira de Almeida (up202004513)

## Introduction

Here, we explore the implementation of a low-level machine and a simple imperative programming language in Haskell. This assignment bridges the gap between abstract programming concepts and practical application, showcasing the versatility of Haskell in handling complex tasks.

## 1. Machine Implementation

The first part of the assignment involves building a low-level machine with a configuration comprising code, an evaluation stack, and storage. It focuses on executing arithmetic and boolean expressions using a set of instructions like push, add, mult, sub, and more. The task includes defining types for the machine's stack (`Stack`) and state (`State`), and implementing functions such as `createEmptyStack`, `stack2Str`, and an interpreter function `run`.

The `Inst` data type defines instructions for a basic computational machine, including arithmetic (`Add`, `Mult`, `Sub`), boolean operations (`Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`), memory operations (`Fetch`, `Store`), and control flow (`Branch`, `Loop`). `Code` is a sequence of these instructions.

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg |
  Fetch String | Store String |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
```

### a) Stack

The `StackVal` type in represents values that can be stored on a stack, including integers (`IVal`) and booleans (`BVal`). `Stack` is a list of these `StackVal` items, allowing for diverse data types to be handled in a unified stack structure.

```haskell
data StackVal = IVal Integer | BVal Bool deriving (Show, Eq)
type Stack = [StackVal]
```

### b) State

The `State` type in represents the state of the machine, consisting of a `Stack` and a list of variable-value pairs (where variables are strings and values are `StackVal` items). This structure facilitates tracking both the current stack and variable bindings in the machine's memory.

```haskell
type State = (Stack, [(String, StackVal)])
```

### c) Create Empty Stack

The `createEmptyStack` function in is defined to initialize an empty stack, returning an empty list `[]`. This function is used to create a new, empty `Stack` for use in the machine's state.

```haskell
createEmptyStack :: Stack
createEmptyStack = []
```

### d) Create Empty State

The `createEmptyState` function in sets up an initial, empty state for the machine. It combines an empty stack, created by `createEmptyStack`, with an empty list for variable-value pairs, forming a complete but empty `State` tuple. This function is essential for initializing the machine's state with no pre-existing data.

```haskell
createEmptyState :: State
createEmptyState = (createEmptyStack, [])
```

### e) Stack to String

The `stack2Str` function converts a stack into a comma-separated string representation. It utilizes `showStackVal` to transform each `StackVal` item (integers, booleans, or strings) into its corresponding string form. The function `showStackVal` specifically handles the conversion, turning integer values (`IVal`) into their numeric strings, and boolean values (`BVal`) into `"True"` or `"False"`.

```haskell
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showStackVal stack)

showStackVal :: StackVal -> String
showStackVal (IVal n) = show n
showStackVal (BVal True) = "True"
showStackVal (BVal False) = "False"
```

### f) State to String

The `state2Str` function converts the state's storage into a string representation. It formats each variable-value pair in the storage as "variable=value," sorting them alphabetically by variable name. This function handles different types of values: integers (`IVal`) and booleans (`BVal`) converting each to its appropriate string format.

```haskell
state2Str :: State -> String
state2Str (_, store) =
    intercalate "," . map showVarVal . sortBy (comparing fst) $ store
  where
    showVarVal :: (String, StackVal) -> String
    showVarVal (var, IVal n) = var ++ "=" ++ show n
    showVarVal (var, BVal True) = var ++ "=True"
    showVarVal (var, BVal False) = var ++ "=False"
```

## 2. Programming Language Development

This part of the assignment involves creating a small imperative programming language that compiles into instructions for the previously defined low-level machine. It includes arithmetic and boolean expressions, assignments, sequences, conditionals, and loops. Key tasks are defining Haskell data types (`Aexp` for arithmetic expressions, `Bexp` for boolean expressions, and `Stm` for statements) and developing a compiler to translate these high-level constructs into the low-level machine's code. Additionally, a parser is required to convert string representations of programs into structured data, adhering to specific syntactic rules.

### a) Data types that define the structure of the imperative programming language


'Aexp' data type models arithmetic expressions with integer literals (`ALit`), variables (`AVar`), and operations like addition (`AAdd`), subtraction (`ASub`), multiplication (`AMul`), and division (`ADiv`).

```haskell
data Aexp = ALit Integer
          | AVar String
          | AAdd Aexp Aexp
          | ASub Aexp Aexp
          | AMul Aexp Aexp
          | ADiv Aexp Aexp

          deriving Show
```

`Bexp` data type represents boolean expressions. It includes boolean literals (`BLit`), equality (`BEq`) and less-than-or-equal comparisons (`BLe`) between arithmetic expressions, logical operations like and (`BAnd`), or (`BOr`), and negation (`BNot`), as well as constants for true (`BTrue`) and false (`BFalse`).

```haskell
data Bexp = BLit Bool
          | BEq Aexp Aexp
          | BLe Aexp Aexp
          | BAnd Bexp Bexp
          | BOr Bexp Bexp
          | BNot Bexp
          deriving Show
```

`Stm` describes statements in the language, including variable assignment, sequences of statements, conditional if-then-else structures, while loops, and a noop (no operation) statement.

```haskell
data Stm = SAssign String Aexp
         | SSeq Stm Stm
         | SIf Bexp Stm Stm
         | SWhile Bexp Stm
         | Noop
         deriving Show
```

### b) Compiler

`compileAexp` translates arithmetic expressions (`Aexp`) into machine code, handling literals, variables and basic arithmetic operations. 

```haskell
compileAexp :: Aexp -> Code
compileAexp (ALit n) = [Push n]
compileAexp (AVar x) = [Fetch x]
compileAexp (AAdd a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Add]
compileAexp (ASub a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Sub]
compileAexp (AMul a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Mult]
```

`compileBexp` translates boolean expressions (`Bexp`) into a sequence of machine code instructions. It handles boolean literals (`BLit`), compares arithmetic expressions for equality (`BEq`) and less-than-or-equal (`BLe`), performs logical operations like and (`BAnd`), and applies negation (`BNot`). Additionally, it directly translates boolean constants true (`BTrue`) and false (`BFalse`) into their respective instructions. 

```haskell
compileBexp :: Bexp -> Code
compileBexp (BLit b) = [if b then Tru else Fals]
compileBexp (BEq a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Equ]
compileBexp (BLe a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Le]
compileBexp (BAnd b1 b2) = compileBexp b1 ++ compileBexp b2 ++ [And]
compileBexp (BNot b) = compileBexp b ++ [Neg]
compileBexp (BTrue) = [Tru]
compileBexp (BFalse) = [Fals]
```

`compileStm` converts statements (`Stm`) like assignments, sequences, conditionals, and loops into code. 

```haskell
compileStm :: Stm -> Code
compileStm (SAssign x a) = compileAexp a ++ [Store x]
compileStm (SSeq s1 s2) = compileStm s1 ++ compileStm s2
compileStm (SIf b s1 s2) = compileBexp b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (SWhile b s) = [Loop (compileBexp b) (compileStm s)]
```

The main `compile` function aggregates these conversions for a list of statements, forming the compiled program.

```haskell
compile :: [Stm] -> Code
compile statements = concatMap compileStm statements
```

### c) Parser

This parser translates a program written in a simple language into a structured format that can be further processed or executed, such as compiling into machine code or interpreting directly. The parser handles various elements of programming language syntax, including expressions, control flow statements, and logical operations.

`lexer` splits the input string into tokens, recognizing spaces, alphabets, digits, and special symbols like `:=`, `==`, `<=`, `&&`, `||`, and other single-character tokens.

```haskell
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = let (token, rest) = span isAlpha (c:cs) in token : lexer rest
  | isDigit c = let (token, rest) = span isDigit (c:cs) in token : lexer rest
  | c == ':' && not (null cs) && head cs == '=' = ":=" : lexer (tail cs)
  | c == '=' && not (null cs) && head cs == '=' = "==" : lexer (tail cs)
  | c == '=' = "=" : lexer cs -- This handles single '=' which should be part of '==', etc.
  | c == '<' && not (null cs) && head cs == '=' = "<=" : lexer (tail cs)
  | c == '&' && not (null cs) && head cs == '&' = "&&" : lexer (tail cs)
  | c == '|' && not (null cs) && head cs == '|' = "||" : lexer (tail cs)
  | c `elem` "+-*/:;(){}" = [c] : lexer cs
  | otherwise = error $ "Unexpected character: " ++ [c]
```

`parseStm` and `parseStm'` parse statements, supporting sequences of statements separated by semicolons and handling the end of input.

```haskell
parseStm :: [String] -> Either String (Stm, [String])
parseStm [] = Right (Noop, [])
parseStm tokens = parseStm' tokens []

parseStm' :: [String] -> [Stm] -> Either String (Stm, [String])
parseStm' [] stms = Right (foldr1 SSeq (reverse stms), [])
parseStm' tokens stms = do
  (stm, remainingTokens) <- parseStmPart tokens
  -- If there are no more tokens after a statement, it's the end of input
  if null remainingTokens
    then Right (foldr1 SSeq (reverse (stm : stms)), [])
    else case remainingTokens of
      ";" : rest -> parseStm' rest (stm : stms)
      -- Handle the case where semicolon is missing
      _ -> Left $ "parseStm': expected semicolon after statement, got " 
```

`parseStmPart` parses individual parts of statements, such as assignments, if conditions, and while loops.

```haskell
parseStmPart :: [String] -> Either String (Stm, [String])
parseStmPart [] = Left "parseStmPart: unexpected end of input"
parseStmPart ("if" : rest) = parseIf ("if" : rest)
parseStmPart ("while" : rest) = parseWhile ("while" : rest)
parseStmPart (var : ":=" : rest) = do
  (expr, rest') <- parseAexp rest
  Right (SAssign var expr, rest')
parseStmPart unexpected = Left $ "Unexpected statement: " ++ unwords unexpected
```

`parseAexp`, `parseAddSub`, `parseMulDiv`, `parseTerm` parse arithmetic expressions, handling operations like addition, subtraction, multiplication, and division, as well as parentheses, literals, and variables.

```haskell
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
    [] -> Left "parseTerm: missing closing parenthesis"
    (")":moreTokens) -> Right (exp, moreTokens)
    _ -> Left "parseTerm: missing closing parenthesis"
parseTerm (x:xs)
  | all isDigit x = Right (ALit (read x), xs)
  | isAlpha (head x) && all isLower x = Right (AVar x, xs)
  | otherwise = Left $ "parseTerm: unexpected token " ++ x
```

`parseComplexBexp` parses complex boolean expressions, dealing with logical operators, negation, and comparison operations.

```haskell
parseComplexBexp :: [String] -> Either String (Bexp, [String])
parseComplexBexp tokens = do
    if head tokens == "not"
          then if length tokens > 1 && head (tail tokens) == "("
              then do
                  let matchingIndex = findMatchingIndex tokens 0 0
                      (innerBexp, remaining) = splitAt (matchingIndex + 1) tokens
                  (parsedBexp, remaining) <- parseComplexBexp (tail innerBexp)
                  Right (BNot parsedBexp, remaining)
              else do
                  (bexp, remaining) <- parseComplexBexp (tail tokens)
                  Right (BNot bexp, remaining)
    else if "(" `elem` tokens && ")" `elem` tokens
      then if head tokens == "(" && last tokens == ")"
        then parseComplexBexp (init (tail tokens))  -- Removes the outer parentheses and retries
        else Left "Unmatched parentheses" 
    else if "and" `elem` tokens
      then do
        let (beforeAnd, (_:afterAnd)) = break (== "and") tokens
        (exp1, _) <- parseComplexBexp beforeAnd
        (exp2, remaining) <- parseComplexBexp afterAnd
        let comparison = BAnd exp1 exp2
        if null remaining
          then Right (comparison, remaining)
          else do
            (restOfBexp, finalTokens) <- parseComplexBexp remaining
            Right (comparison, finalTokens)
    else if head tokens == "true"
      then Right (BTrue, tail tokens)
    else if head tokens == "false"
      then Right (BFalse, tail tokens)
    else if "==" `elem` tokens || "<=" `elem` tokens
      then do
        (operator, before, after) <- parseOperator tokens
        case operator of
          "==" -> do
            (exp1, tokensAfterExp1) <- parseAexp before
            (exp2, remaining) <- parseAexp after
            let comparison = BEq exp1 exp2
            if null remaining
              then Right (comparison, remaining)
              else do
                (restOfBexp, finalTokens) <- parseComplexBexp remaining
                Right (comparison, finalTokens)
          "<=" -> do
            (exp1, tokensAfterExp1) <- parseAexp before
            (exp2, remaining) <- parseAexp after
            let comparison = BLe exp1 exp2
            if null remaining
              then Right (comparison, remaining)
              else do
                (restOfBexp, finalTokens) <- parseComplexBexp remaining
                Right (comparison, finalTokens)
          _ -> error "Unknown comparison operator"
{-     else if head tokens == "not"
        then if length tokens > 1 && head (tail tokens) == "("
            then do
                let matchingIndex = findMatchingIndex tokens 0 0
                    (innerBexp, remaining) = splitAt matchingIndex tokens
                trace ("InnerBexp: " ++ show innerBexp ++ ", Remaining: " ++ show remaining) $
                    do
                        (parsedBexp, remaining) <- parseComplexBexp (init (tail innerBexp))
                        Right (BNot parsedBexp, remaining)
            else do
                (bexp, remaining) <- parseComplexBexp (tail tokens)
                Right (BNot bexp, remaining) -}
    else
        Left "Unknown operator"

      {- else do
        (operator, before, after) <- parseOperator tokens
        case operator of
          "not" -> do
            (bexp, remaining) <- parseComplexBexp after
            Right (BNot bexp, remaining)
          _ -> error "Unknown operator" -}
```

`parseOperator` identifies operators within tokens and categorizes them for further processing.

```haskell
parseOperator :: [String] -> Either String (String, [String], [String])
parseOperator [] = Left "Expected a comparison operator, but got an empty list."
parseOperator (op:rest)
  | op `elem` ["==", "<=", "and", "not"] = Right (op, [], rest)
  | otherwise = case parseOperator rest of
                  Right (operator, before, after) -> Right (operator, op:before, after)
                  Left err -> Left err
```

`parseIf` and `parseWhile` handle parsing of if and while constructs, respectively, including their conditions and body statements.

```haskell
parseIf :: [String] -> Either String (Stm, [String])
parseIf ("if":rest) = do
    let (conditionTokens, thenTokens, elseTokens) = extractInsideCodeIf rest
    (condition, _) <- parseComplexBexp conditionTokens
    (thenStatement, _) <- parseStm thenTokens
    (elseStatement, remaining) <- parseStm elseTokens
    Right (SIf condition thenStatement elseStatement, remaining)
parseIf _ = Left "Invalid input to parseIf"

parseWhile :: [String] -> Either String (Stm, [String])
parseWhile ("while":tokens) = do
    let (conditionTokens, doTokens) = extractInsideCodeWhile tokens
    (condition, restConditional) <- parseComplexBexp conditionTokens
    (bodyStatement, rest) <- parseStm doTokens
    Right (SWhile condition bodyStatement, rest)
```

`parse` is the main parsing function that applies the lexer to the input string and then processes the tokens to form a list of statements.

```haskell
parse :: String -> [Stm]
parse str = unsafePerformIO $ do
  let tokens = lexer str
  putStrLn $ "Tokens in parse: " ++ show tokens
  parseUntilEmpty tokens []
  where
    parseUntilEmpty :: [String] -> [Stm] -> IO [Stm]
    parseUntilEmpty [] parsed = return parsed
    parseUntilEmpty remainingTokens parsed = do
      case parseStm remainingTokens of
        Left err -> error $ "Parsing error: " ++ err
        Right (stm, newRemaining) -> parseUntilEmpty newRemaining (parsed ++ [stm])
```

