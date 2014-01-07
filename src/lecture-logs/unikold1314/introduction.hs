{-

A Haskell transcription of introduction.pro.
The transcription is highly mechanic.
Some names had to be renamed to appeal to Haskell conventions.
Some equations were rephrased to appeal to Haskell idioms.
The Maybe type constructor is used to deal with partiality.

Sample program in a simple expression-oriented language:

if 0=0 then true else 2*3

Let's defined syntax, type system, and semantics for that language.

Syntax in BNF:

expr = const 
     | expr op expr 
     | "if" expr "then" expr "else" expr
op = "*" | "="
const = "true" | "false" | int

Let's switch to Haskell.

-}

-- Syntax of expressions
data Expr 
  = Const Const
  | Binary Expr Expr Op
  | If Expr Expr Expr
  deriving (Eq, Show, Read)

-- Binary operators
data Op = Mult | Eq
  deriving (Eq, Show, Read)

-- Constants
data Const = BoolConst Bool | IntConst Int
  deriving (Eq, Show, Read)

-- A well-typed sample
wellTypedSample =
  If
    (Binary (Const (IntConst 0)) (Const (IntConst 0)) Eq)
    (Const (IntConst 42))
    (Binary (Const (IntConst 2)) (Const (IntConst 3)) Mult)

-- An ill-typed sample
illTypedSample =
  If 
    (Binary (Const (IntConst 0)) (Const (IntConst 0)) Eq)
    (Const (BoolConst True))
    (Binary (Const (IntConst 2)) (Const (IntConst 3)) Mult)

-- Syntax of types
data Type = BoolType | IntType 
  deriving (Eq, Show, Read)

-- Predicate for well-typedness
wellTyped :: Expr -> Maybe Type
wellTyped (Const (BoolConst _)) = Just BoolType
wellTyped (Const (IntConst _)) = Just IntType
wellTyped (Binary e1 e2 Mult)
  = case (wellTyped e1, wellTyped e2) of
      (Just IntType, Just IntType) -> Just IntType
      _ -> Nothing
wellTyped (Binary e1 e2 Eq)
  = case (wellTyped e1, wellTyped e2) of
      (Just t1, Just t2) -> 
        if t1==t2
          then Just BoolType
          else Nothing
      _ -> Nothing
wellTyped (If e1 e2 e3)
  = case (wellTyped e1, wellTyped e2, wellTyped e3) of
      (Just BoolType, Just t1, Just t2) ->
        if t1==t2 
          then Just t1
          else Nothing
      _ -> Nothing

{-

For example:

*Main> wellTyped wellTypedSample
Just IntType
*Main> wellTyped illTypedSample 
Nothing

-}

-- Syntax of values
value (Const _) = True
value _ = False

-- Big-step semantics of expressions
eval :: Expr -> Const
eval (Const x) = x
eval (Binary e1 e2 Mult)
  = let
      (IntConst c1) = eval e1
      (IntConst c2) = eval e2
    in
      IntConst (c1 * c2)
eval (Binary e1 e2 Eq)
  = BoolConst (eval e1 == eval e2)
eval (If e1 e2 e3)
  = let
      (BoolConst c) = eval e1
    in
      if c 
        then eval e2
        else eval e3

{-

For example:

*Main> eval wellTypedSample 
IntConst 42

-}

-- Combine well-typedness and evaluation
wellTypedEval :: Expr -> Maybe Const
wellTypedEval e
  = case wellTyped e of
      Just _ -> Just (eval e)
      Nothing -> Nothing
