import Data.Char (toLower, isDigit, isAlpha, isLower,isSpace,isAlphaNum)
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
    trace ("- Push " ++ show n ++ "\tStack: " ++ stack2Str (IVal n : stack)) $
    run (code, IVal n : stack, state)
run (Add:code, IVal n1 : IVal n2 : stack, state) =
    trace ("- Add " ++ show (n1 + n2) ++ "\tStack: " ++ stack2Str (IVal (n1 + n2) : stack)) $
    run (code, IVal (n1 + n2) : stack, state)
run (Sub:code, IVal n1 : IVal n2 : stack, state) =
    trace ("- Sub " ++ show (n1 + n2) ++ "\tStack: " ++ stack2Str (IVal (n1 + n2) : stack)) $
    run (code, IVal (n1 - n2) : stack, state)
run (Mult:code, IVal n1 : IVal n2 : stack, state) =
    trace ("- Mult " ++ show (n1 + n2) ++ "\tStack: " ++ stack2Str (IVal (n1 + n2) : stack)) $
    run (code, IVal (n1 * n2) : stack, state)
run (Tru:code, stack, state) =
    trace ("- Tru \tStack: " ++ stack2Str (BVal True : stack)) $
    run (code, BVal True : stack, state)
run (Fals:code, stack, state) =
    trace ("- Fals \tStack: " ++ stack2Str (BVal True : stack)) $
    run (code, BVal False : stack, state)
run ((Store var):code, val:stack, (s, store)) =
    let updatedStore = updateStore var val store
    in trace ("- Store " ++ var ++ " " ++ showStackVal val ++ "\t Stack: " ++ stack2Str (val : stack)) $
       run (code, stack, (s, updatedStore))
  where
    updateStore var val [] = [(var, val)]
    updateStore var val ((v,sVal):vs)
      | v == var  = (var, val) : vs
      | otherwise = (v, sVal) : updateStore var val vs
run ((Fetch varName):code, stack, state@(_, store)) =
    case lookup varName store of
        Just val -> trace ("-Fetch " ++ varName ++ "\tStack: " ++ stack2Str (val : stack)) $ 
                    run (code, val : stack, state)
        Nothing  -> error "Run-time error"  -- Adjusted error message to match the requirement
run (Neg:code, BVal b : stack, state) =
    trace ("- Neg: "  ++ "\tStack: " ++ stack2Str (BVal (not b) : stack)) $
    run (code, BVal (not b) : stack, state)
run (Neg:code, stack, state) =
    error "Neg instruction expects a boolean value on top of the stack"
run (Equ:code, v1 : v2 : stack, state) =
    trace ("- Equ "  ++ "\tStack: " ++ stack2Str (BVal (v1 == v2): stack)) $
    run (code, BVal (v1 == v2) : stack, state)
run (Le:code, IVal n1 : IVal n2 : stack, state) =
    trace ("- Le "  ++ "\tStack: " ++ stack2Str (BVal (n1 <= n2): stack)) $
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
    trace ("- And: "++ "\tStack: " ++ stack2Str (BVal (b1 && b2): stack)) $
    run (code, BVal (b1 && b2) : stack, state)
run (And:_, _, _) =
    error "Runtime error: 'And' operation requires two boolean values on top of the stack"
run ((Branch condCode thenCode):restCode, stack, state) =
  case run (condCode, stack, state) of
    (_, BVal True : stack', state') -> run (condCode ++ restCode, stack', state')
    (_, BVal False : stack', state') -> run (thenCode ++ restCode, stack', state') -- No 'elseCode' in this case
    _ -> error "Branch condition did not evaluate to a boolean"
run (inst : restCode, stack, state) =
  error $ "Unhandled instruction: " ++ show inst


-- Implement other instructions as needed.



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)




main :: IO ()
main = do
  putStrLn "Enter program code:"
  input <- getLine
  let finalState = runProgram input
      formattedState = formatState finalState
  putStrLn formattedState

formatState :: State -> String
formatState (_, store) = intercalate "," $ map (\(var, val) -> var ++ "=" ++ showStackVal val) store

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
          | BTrue
          | BFalse
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


compileBexp :: Bexp -> Code
compileBexp (BLit b) = [if b then Tru else Fals]
compileBexp (BEq a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Equ]
compileBexp (BLe a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Le]
compileBexp (BAnd b1 b2) = compileBexp b2 ++ compileBexp b1 ++ [And]
compileBexp (BNot b) = compileBexp b ++ [Neg]
compileBexp (BTrue) = [Tru]
compileBexp (BFalse) = [Fals]

-- Compiles a single statement into Code
compileStm :: Stm -> Code
compileStm (SAssign x a) = compileAexp a ++ [Store x]
compileStm (SSeq s1 s2) = compileStm s1 ++ compileStm s2
compileStm (SIf b s1 s2) = compileBexp b ++ [Branch (compileStm s1) (compileStm s2)]
compileStm (SWhile b s) = [Loop (compileBexp b) (compileStm s)]
compileStm Noop = [] 


-- Compiles a list of statements into Code
compile :: [Stm] -> Code
compile statements = concatMap compileStm statements



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
      _ -> Left $ "parseStm': expected semicolon after statement, got " ++ show remainingTokens

-- SSeq should be a statement that represents a sequence of statements
parseStmPart :: [String] -> Either String (Stm, [String])
parseStmPart [] = Left "parseStmPart: unexpected end of input"
-- parseStmPart ("if" : rest) = parseIf ("if" : rest)
parseStmPart ("if" : rest) = do
    (ifStm, remaining) <- parseIf ("if" : rest)
    trace ("Remaining after parsing 'if': " ++ show remaining) $ do
        (restStm, remaining') <- parseStm remaining
        Right (SSeq ifStm restStm, remaining')
parseStmPart ("while" : rest) = parseWhile ("while" : rest)
parseStmPart (var : ":=" : rest) = do
  (expr, rest') <- parseAexp rest
  Right (SAssign var expr, rest')
parseStmPart unexpected = Left $ "Unexpected statement: " ++ unwords unexpected

{- 
parseAexp :: [String] -> Either String (Aexp, [String])
parseAexp tokens = parseAddSub tokens -}

parseAexp :: [String] -> Either String (Aexp, [String])
parseAexp tokens =
  if not (null tokens) && head tokens == "(" && last tokens == ")"
    then parseAddSub (init (tail tokens))
    else parseAddSub tokens


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

takeUntil :: Eq a => a -> [a] -> ([a], [a])
takeUntil delim tokens =
  let (beforeDelim, afterDelim) = span (/= delim) tokens
  in (beforeDelim, drop 1 afterDelim)

{- extractInsideCodeIf :: [String] -> ([String], [String], [String],[String])
extractInsideCodeIf tokens =
  let (conditionTokens, afterCondition) = takeUntil "then" tokens
      (thenTokens, afterThen) = takeUntil "else" afterCondition
      elseTokens = if not (null afterThen) && head afterThen == "("
                       then let closingIndex = findMatchingIndex afterThen 0 0
                            in if closingIndex > 0
                                  then init (tail (take (closingIndex + 1) afterThen)) 
                                  else []
                       else takeWhile (/= ";") afterThen
      afterElse = drop (length elseTokens + 1) afterThen
      thenTokens' = if not (null thenTokens) && head thenTokens == "("
                       then init (tail thenTokens)
                       else thenTokens
  in (conditionTokens, thenTokens', elseTokens,afterElse) -}

extractInsideCodeIf :: [String] -> ([String], [String], [String], [String])
extractInsideCodeIf tokens =
  let (conditionTokens, afterCondition) = takeUntil "then" tokens
      (thenTokens, afterThen) = takeUntil "else" afterCondition
      (elseTokens, afterElse) = if not (null afterThen) && head afterThen == "("
                                    then let closingIndex = findMatchingIndex afterThen 0 0
                                             elseTokens' = if closingIndex > 0
                                                                then init (tail (take (closingIndex + 1) afterThen)) 
                                                                else []
                                             afterElse' = drop (length (take (closingIndex + 1) afterThen) + 1) afterThen
                                         in (elseTokens', afterElse')
                                    else (takeWhile (/= ";") afterThen, dropWhile (/= ";") afterThen)
      thenTokens' = if not (null thenTokens) && head thenTokens == "("
                       then init (tail thenTokens)
                       else thenTokens
      afterElse' = if not (null afterThen) && head afterThen == "("
                      then afterElse
                      else drop (length elseTokens + 1) afterThen
  in do(conditionTokens, thenTokens', elseTokens, afterElse')



parseIf :: [String] -> Either String (Stm, [String])
parseIf ("if":rest) = do
    let (conditionTokens, thenTokens, elseTokens,afterElse) = extractInsideCodeIf rest
    (condition, _) <- parseComplexBexp conditionTokens
    (thenStatement, thenRemaining) <- parseStm thenTokens
    (elseStatement, elseRemaining) <- parseStm elseTokens
    Right (SIf condition thenStatement elseStatement, afterElse)



findMatchingIndex :: [String] -> Int -> Int -> Int
findMatchingIndex tokens count index
  | null tokens && count /= 0 = error "No matching ending parentheses."
  | head tokens == "(" = findMatchingIndex (tail tokens) (count + 1) (index + 1)
  | head tokens == ")" = if count == 1 then index else findMatchingIndex (tail tokens) (count - 1) (index + 1)
  | otherwise = findMatchingIndex (tail tokens) count (index + 1)


extractInsideCodeWhile :: [String] -> ([String], [String])
extractInsideCodeWhile tokens = 
  let (conditionTokens, afterCondition) = takeUntil "do" tokens
      closingIndex = findMatchingIndex afterCondition 0 0
      doTokens = if closingIndex > 0
                     {- then take (closingIndex + 1) afterCondition -}
                    then init (tail (take (closingIndex + 1) afterCondition)) 
                    else []
  in (conditionTokens,doTokens)   

parseWhile :: [String] -> Either String (Stm, [String])
parseWhile ("while":tokens) = do
    let (conditionTokens, doTokens) = extractInsideCodeWhile tokens
    (condition, restConditional) <- parseComplexBexp conditionTokens
    (bodyStatement, rest) <- parseStm doTokens
    Right (SWhile condition bodyStatement, rest)

{- parseComplexBexp :: [String] -> Either String (Bexp, [String])
parseComplexBexp tokens = do
  (exp1, tokensAfterExp1) <- parseAexp tokens
  (operator, rest) <- parseOperator tokensAfterExp1
  (exp2, remaining) <- parseAexp rest
  let comparison = case operator of
        "==" -> BEq exp1 exp2
        "<=" -> BLe exp1 exp2
        "and" -> BAnd exp1 exp2
        _    -> error "Unknown comparison operator"
  if null remaining
    then Right (comparison, remaining)
    else do
      (restOfBexp, finalTokens) <- parseComplexBexp remaining
      Right (BAnd comparison restOfBexp, finalTokens)
     -}
parseComplexBexp :: [String] -> Either String (Bexp, [String])
parseComplexBexp tokens = do
    if head tokens == "(" && last tokens == ")"
      then do parseComplexBexp (init (tail tokens)) 
    else if "and" `elem` tokens || "=" `elem` tokens
      then do
        let (beforeAnd, (_:afterAnd)) = break (`elem` ["and", "="]) tokens
        (exp1, _) <- parseComplexBexp beforeAnd
        (exp2, remaining) <- parseComplexBexp afterAnd
        let comparison = BAnd exp1 exp2
        if null remaining
          then Right (comparison, remaining)
          else do
            (restOfBexp, finalTokens) <- parseComplexBexp remaining
            Right (comparison, finalTokens)
    else if head tokens == "not"
          then if length tokens > 1 && head (tail tokens) == "("
              then do
                  let matchingIndex = findMatchingIndex tokens 0 0
                      (innerBexp, remaining) = splitAt (matchingIndex + 1) tokens
                  (parsedBexp, remaining) <- parseComplexBexp (tail innerBexp)
                  Right (BNot parsedBexp, remaining)
              else do
                  (bexp, remaining) <- parseComplexBexp (tail tokens)
                  Right (BNot bexp, remaining)
    else if head tokens == "True"
      then Right (BTrue, tail tokens)
    else if head tokens == "False"
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
      Left $ "Unknown opersator: " ++ head tokens

      {- else do
        (operator, before, after) <- parseOperator tokens
        case operator of
          "not" -> do
            (bexp, remaining) <- parseComplexBexp after
            Right (BNot bexp, remaining)
          _ -> error "Unknown operator" -}


parseOperator :: [String] -> Either String (String, [String], [String])
parseOperator [] = Left "Expected a comparison operator, but got an empty list."
parseOperator (op:rest)
  | op `elem` ["==", "<=", "and", "not"] = Right (op, [], rest)
  | otherwise = case parseOperator rest of
                  Right (operator, before, after) -> Right (operator, op:before, after)
                  Left err -> Left err

{- parseOperator :: [String] -> Either String (String, [String])
parseOperator ("==":rest) = Right ("==", rest)
parseOperator ("<=":rest) = Right ("<=", rest)
parseOperator ("and":rest) = Right ("and", rest)
parseOperator ("not":rest) = Right ("not", rest)

parseOperator tokens = Left $ "Expected a comparison operator, but got: " ++ show tokens -}

{- parseBexpTokens ("not":rest) = do
  (bexp, remaining) <- parseBexpTokens rest
  Right (BNot bexp, remaining)
parseBexpTokens (x:"and":xs) = do
  (b1, remaining1) <- parseBexpTokens [x]
  (b2, remaining2) <- parseBexpTokens xs
  Right (BAnd b1 b2, remaining2)

 -}


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

runProgram :: String -> State
runProgram programCode = finalState
  where
    instructions = parse programCode
    (compiledProg, _, finalState) = do
      let compiledProg = compile instructions
          stack = createEmptyStack
          state = createEmptyState
      trace ("Compiled Program: " ++ show compiledProg) $ run (compiledProg, stack, state)


testParser :: String -> (String, String)
testParser programCode = (instructionStr, finalStateStr)
  where
    instructions = parse programCode
    instructionStr = trace ("Instructions generated from parsing: " ++ show instructions) ""
    (_, _, finalState) = run (compile instructions, createEmptyStack, createEmptyState)
    finalStateStr = state2Str finalState

tests :: IO ()
tests = do
    putStrLn ""
    let testCases =
          [ ("x := 5; x := x - 1;", ("", "x=4"))
          , ("x := 0 - 2;", ("", "x=-2"))
          , ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("", "y=2"))
          , ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);", ("", "x=1"))
          , ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("", "x=2"))
          , ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("", "x=2,z=4"))
          , ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;", ("", "x=34,y=68"))
          , ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;", ("", "x=34"))
          , ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;", ("", "x=1"))
          , ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;", ("", "x=2"))
          , ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("", "x=2,y=-10,z=6"))
          , ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("", "fact=3628800,i=1"))
          ]

    mapM_ (\(testCase, expected) -> do
            let (instructionStr, finalStateStr) = testParser testCase
                testResult = if (instructionStr, finalStateStr) == expected
                    then "--------------------------------\nTest Passed\n--------------------------------\n"
                    else "--------------------------------\nTest Failed\n--------------------------------\n"
                output = "Test: " ++ testCase ++ " -> " ++ testResult ++ " - Expected: " ++ show expected ++ " | Got: " ++ "(" ++ instructionStr ++ ", " ++ finalStateStr ++ ")"
            putStrLn output
        ) testCases