{-

// A Java program used as a starting point

import java.util.Scanner;

public class Demo {
	
	// Compute factorial of a number
	public static void main(String[] args) {
		// Read argument from stdin
		Scanner in = new Scanner(System.in);
		// Actual computation of factorial
		int n = in.nextInt();
		int r = 1;
		while (n>0) r *= n--;
		// Write result to stdout
		System.out.println(r);
	}

}

// Let us simplify the language at hand.
// Here is the same program in a conceived language While.

int n; 
read n;
int r;
r = 1;
while (n>0) do 
  r = r * n;
  n = n - 1;
od;
write r;

// Let's agree on what are the new challenges compared to FL:
// * input/output (as one kind of side effect)
// * imperative variables (as another kind of side effect)
// * while loop (also intrinsically imperative)

-}

-- The expression syntax
-- We do not support read/write at this point.
data Expr = IntConst Int
          | Var String
          | Binary Expr Expr Op
 deriving (Read, Show, Eq)

-- Binary operators
data Op = Greater | Mult | Sub
 deriving (Read, Show, Eq)

-- The statement syntax
data Stm = Skip
         | IntDecl String
         | Assign String Expr
         | Seq Stm Stm
         | If Expr Stm Stm
         | While Expr Stm
 deriving (Read, Show, Eq)

-- An "Ok" sample
okSample
  = Seq (IntDecl "n") (
    Seq (Assign "n" (IntConst 5)) (
    Seq (IntDecl "r") (
    Seq (Assign "r" (IntConst 1)) (
    (While 
      (Binary (Var "n") (IntConst 0) Greater)
      (Seq
        (Assign "r" (Binary (Var "r") (Var "n") Mult))
        (Assign "n" (Binary (Var "n") (IntConst 1) Sub))
      )
    )))))

-- States (mapping variable ids to values)
type State = String -> Value

-- Values (i.e., results of expression evaluation)
data Value = IntValue Int | BoolValue Bool
 deriving (Read, Show, Eq)

-- Expression evaluation
eval :: Expr -> State -> Value
eval (IntConst i) = const (IntValue i)
eval (Var x) = \s -> s x
eval (Binary e1 e2 o) = \s -> 
  evalOp o (eval e1 s) (eval e2 s)

-- Evaluation of binary operators
evalOp :: Op -> Value -> Value -> Value 
evalOp Greater (IntValue i1) (IntValue i2) = BoolValue (i1 > i2)
evalOp Mult (IntValue i1) (IntValue i2) = IntValue (i1 * i2)
evalOp Sub (IntValue i1) (IntValue i2) = IntValue (i1 - i2)

-- Execution semantics of statements
exec :: Stm -> State -> State
exec (IntDecl _) = id
exec (Assign x e)
  = \s -> \x' -> if x==x'
                   then (eval e s)
                   else s x'
exec Skip = id
exec (Seq s1 s2) = exec s2 . exec s1
exec (If e s1 s2)
  = cond (eval e) (exec s1) (exec s2)
exec (While e s)
  = fix f
    where
      f g = cond (eval e) (g . exec s) id

{-

Here is a non-compositional semantics.

exec (While e s)
  = exec (If e (Seq s (While e s)) Skip)


-}

-- Helper
cond a b c s = 
  case (a s) of
    (BoolValue True) -> b s
    (BoolValue False) -> c s

-- Helper
fix f = f (fix f)

-- Compute factorial of 5
main = do
  print $ exec okSample undefined "r"

{-

> main
IntValue 120

-}
