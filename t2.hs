-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- createEmptyStack :: Stack
createEmptyStack = undefined -- TODO, Uncomment the function signature after defining Stack

-- stack2Str :: Stack -> String
stack2Str = undefined -- TODO, Uncomment all the other function type declarations as you implement them

-- createEmptyState :: State
createEmptyState = undefined -- TODO, Uncomment the function signature after defining State

-- state2Str :: State -> String
state2Str = undefined -- TODO

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")   good 
-- testParser "x := 0 - 2;" == ("","x=-2")   good
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")   good
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34") good
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6") good
-- testParser "v" == ("","fact=3628800,i=1")


{- Test compile
main :: IO ()
main = do
    let stm1 = SAssign "x" (ALit 5)  -- Assign the value 5 to variable "x"
        stm2 = SAssign "y" (ALit 3)  -- Assign the value 3 to variable "y"
        stm3 = SIf
            (BEq (AVar "x") (AVar "y"))
            (SAssign "result" (ALit 1))  -- Assign an arithmetic expression based on the condition
            (SAssign "result" (ALit 0))  -- Assign a different arithmetic expression based on the else branch

        stm4 = SWhile (BLe (AVar "x") (ALit 10)) (SAssign "x" (AAdd (AVar "x") (ALit 1)))

    let program = [stm1, stm2, stm3, stm4]

    putStrLn "Testing compileStm and compile:"
    putStrLn "---------------------------------"

    let generatedCode = compile program

    -- Print the generated code
    putStrLn "Generated Code:"
    print generatedCode
-}
{- Test part 1
  let testResult = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
  let (stackStr, stateStr) = testResult -- Unpack the tuple into stackStr and stateStr
  putStrLn "Stack:"
  putStrLn stackStr -- Corrected to use stackStr
  putStrLn "State:"
  putStrLn stateStr -- Corrected to use stateStr
  let expectedResult = ("","fact=3628800,i=1")
  print (testResult == expectedResult) -- This compares the entire tuple
  -- The following lines are likely not necessary if the above print statement is what you want
  print (stackStr == fst expectedResult) -- Compare stack string to expected stack string
  print (stateStr == snd expectedResult) -- Compare state string to expected state string
  -- If you still want to print stackStr and stateStr separately, you can uncomment these lines:
  print stackStr  -- Print the actual stack string
  print stateStr  -- Print the actual state string
-}
{- Test Compiler auxiliar
main :: IO ()
main = do
    -- Testing compileAexp
    let expr1 = ALit 5
        expr2 = ALit (-3)
        expr3 = AVar "x"
        expr4 = AVar "y"
        expr5 = AAdd (ALit 2) (ALit 3)
        expr6 = ASub (ALit 7) (ALit 4)
        expr7 = AMul (ALit 6) (ALit 2)
        expr9 = AAdd (ALit 10) (AMul (ALit 2) (AVar "z"))

    putStrLn "Testing compileAexp:"
    putStrLn "---------------------"
    
    putStrLn "Expression 1:"
    print $ compileAexp expr1
    
    putStrLn "Expression 2:"
    print $ compileAexp expr2
    
    putStrLn "Expression 3:"
    print $ compileAexp expr3
    
    putStrLn "Expression 4:"
    print $ compileAexp expr4
    
    putStrLn "Expression 5:"
    print $ compileAexp expr5
    
    putStrLn "Expression 6:"
    print $ compileAexp expr6
    
    putStrLn "Expression 7:"
    print $ compileAexp expr7
    
    putStrLn "Expression 9:"
    print $ compileAexp expr9

    -- Testing compileBexp
    let boolExpr1 = BLit True
        boolExpr2 = BLit False
        boolExpr3 = BEq (ALit 5) (ALit 5)
        boolExpr4 = BLe (AVar "x") (ALit 10)
        boolExpr5 = BAnd (BLit True) (BLit False)
        boolExpr6 = BNot (BLit True)

    putStrLn "\nTesting compileBexp:"
    putStrLn "---------------------"
    
    putStrLn "Boolean Expression 1:"
    print $ compileBexp boolExpr1
    
    putStrLn "Boolean Expression 2:"
    print $ compileBexp boolExpr2
    
    putStrLn "Boolean Expression 3:"
    print $ compileBexp boolExpr3
    
    putStrLn "Boolean Expression 4:"
    print $ compileBexp boolExpr4
    
    putStrLn "Boolean Expression 5:"
    print $ compileBexp boolExpr5
    
    putStrLn "Boolean Expression 6:"
    print $ compileBexp boolExpr6
-}
{- Lexer Test
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
-}
{- parseAexp text
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
-}
{- parseStm test
main :: IO ()
main = do
    putStrLn "Testing parseStm:"
    putStrLn "-------------------"

    let input1 = ["x", ":=", "5", ";"]
    let input2 = ["y", ":=", "x", "+", "3"]
    let input3 = ["z", ":=", "(", "x", "+", "y", ")", ";"]
    let input4 = ["a", ":=", "5", "+", ";"]  -- Invalid due to missing right operand
    let input5 = ["b", ":=", "5", "7", "+", "3", ";"]  -- Invalid due to unexpected tokens

    putStrLn "Input 1:"
    print (parseStm input1)

    putStrLn "Input 2:"
    print (parseStm input2)

    putStrLn "Input 3:"
    print (parseStm input3)

    putStrLn "Input 4:"
    print (parseStm input4)

    putStrLn "Input 5:"
    print (parseStm input5)
  -}