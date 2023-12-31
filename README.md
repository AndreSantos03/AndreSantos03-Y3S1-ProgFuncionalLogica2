# TP2 Coursework-Haskell

T03_G07

André Bernardo Ferreira Santos (up202108658)

Inês Ferreira de Almeida (up202004513)

## Introduction

Here, we explore the implementation of a low-level machine and a simple imperative programming language in Haskell. This assignment bridges the gap between abstract programming concepts and practical application, showcasing the versatility of Haskell in handling complex tasks.

## 1. Machine Implementation

This first part of the assignment involves building a low-level machine with a configuration comprising code, an evaluation stack, and storage. It focuses on executing arithmetic and boolean expressions using a set of instructions like push, add, mult, sub, and more. The task includes defining types for the machine's stack (`Stack`) and state (`State`), and implementing functions such as `createEmptyStack`, `stack2Str`, and an interpreter function `run`.

The `Inst` data type defines instructions for a basic computational machine, including arithmetic (`Add`, `Mult`, `Sub`), boolean operations (`Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`), memory operations (`Fetch`, `Store`), and control flow (`Branch`, `Loop`). `Code` is a sequence of these instructions.

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | 
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

### e) State to String

The `state2Str` function in converts the state's storage into a string representation. It formats each variable-value pair in the storage as "variable=value," sorting them alphabetically by variable name. This function handles different types of values: integers (`IVal`) and booleans (`BVal`) converting each to its appropriate string format.

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

`Aexp` represents arithmetic expressions, including literals, variables, additions, subtractions, multiplications, divisions, and boolean literals for true and false.

```haskell
data Aexp = ALit Integer
          | AVar String
          | AAdd Aexp Aexp
          | ASub Aexp Aexp
          | AMul Aexp Aexp
          | ADiv Aexp Aexp
          | ATrue
          | AFalse
          deriving Show
```

`Bexp` defines boolean expressions, encompassing literals, equality and inequality comparisons for arithmetic expressions, logical operations like and/or, and negation.

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

`compileAexp` translates arithmetic expressions (`Aexp`) into machine code, handling literals, variables, basic arithmetic operations, and boolean values. 

```haskell
compileAexp :: Aexp -> Code
compileAexp (ALit n) = [Push n]
compileAexp (AVar x) = [Fetch x]
compileAexp (AAdd a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Add]
compileAexp (ASub a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Sub]
compileAexp (AMul a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Mult]
compileAexp (ATrue) = [Tru]
compileAexp (AFalse) = [Fals]
```

`compileBexp` does the same for boolean expressions (`Bexp`), dealing with literals, equality, inequality, logical operations, and negation. 

```haskell
compileBexp :: Bexp -> Code
compileBexp (BLit b) = [if b then Tru else Fals]
compileBexp (BEq a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Equ]
compileBexp (BLe a1 a2) = compileAexp a2 ++ compileAexp a1 ++ [Le]
compileBexp (BAnd b1 b2) = compileBexp b1 ++ compileBexp b2 ++ [And]
compileBexp (BNot b) = compileBexp b ++ [Neg]
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
