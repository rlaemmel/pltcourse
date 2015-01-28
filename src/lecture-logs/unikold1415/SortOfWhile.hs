{-

(C) 2015, Ralf Laemmel

Denotational semantics of a Nielson&Nielson-like While language 

See slides on the course website and Chapter 4 of
http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.pdf

-}

-- Syntactic domains
data Stm = Skip
         | Seq Stm Stm
         | Assign Id Aexp
         | If Bexp Stm Stm
         | While Bexp Stm
         -- Incomplete!
         
type Id = String
data Aexp = Literal Int | Var Id | Dec Aexp -- Incomplete!
data Bexp = Nonzero Aexp -- Incomplete!

-- Semantic domains
type State = Id -> Int
type Adeno = State -> Int
type Bdeno = State -> Bool
type StateT = State -> State

-- Semantics of statements
sdeno :: Stm -> StateT
sdeno Skip = id
sdeno (Seq stm1 stm2) = sdeno stm2 . sdeno stm1
sdeno (Assign i ae) =
  \s i' -> if i==i'
             then adeno ae s
             else s i'
sdeno (If be stm1 stm2) =
  cond (bdeno be) (sdeno stm1) (sdeno stm2)  
sdeno (While be stm) = fix f
  where
    f g = cond (bdeno be) (g . sdeno stm) id

{-

Remember: we do not go for this semantics:

sdeno (While be stm) =
  sdeno (If be (Seq stm (While be stm)) Skip)

We do not go for that semantics because it wouldn't be compositional,
i.e., the meaning of the while statement would not be defined solely
in terms of the meanings of its immediate syntactic
components. Instead, the meaning would be defined in terms of the
meaning of newly constructed term which also includes the term for the
while statement. Thus, the meaning would be defined in a cyclic manner
so that the meaning is not clearly defined in the sense of a
mathematical function.

-}

-- Semantics of arithmetic expressions
adeno :: Aexp -> Adeno
adeno (Literal i) = const i
adeno (Var v) = \s -> s v
adeno (Dec ae) = (\x -> x - 1) . adeno ae 

-- Semantics of Boolean expressions
bdeno :: Bexp -> Bdeno
bdeno (Nonzero ae) = (/=0) . adeno ae

-- A conditional combinator operating on meanings
cond :: Bdeno -> StateT -> StateT -> StateT
cond x y z =
  \s -> if x s
          then y s
          else z s

-- A fixed point combinator
fix f = f (fix f)

-- A sample program
sample =
  Seq
    (Assign "x" (Literal 42))
    (While
      (Nonzero (Var "x"))
      (Assign "x" (Dec (Var "x"))))

-- Testing the semantics
main =
  do
     let deno = sdeno sample
     let state1 = undefined
     let state2 = deno state1
     print $ state2 "x"

{-

*Main> main
0

-}
