module BAWP where

square x = x * x

sumOfSquares x y = square x + square y

--f a = sumOfSquares (a + 1) (a * 2)

absolute x
    | x > 0 = x
    | x == 0 = 0
    | x < 0 = (-x)

absolute' x
    | x < 0 = (-x)
    | otherwise = x

absolute'' x = if x < 0 then (-x)
               else x

infixr 4 >~ 

x >~ y = x > y || x == y
--x >~ y = not (x < y)

-- Exercise 1.1) What is the result printed by the interpreter in response to
-- each expression? Assume that the sequence is to be evaluated in the order in
-- which it is presented.
--
-- a) 10 -> 10
-- b) 5 + 3 + 4 -> 12
-- c) 9 - 1 -> 8
-- d) 6 / 2 -> 3.0
-- e) (2 * 4) + (4 - 6) -> 6
-- f) a = 3 -> nothing is printed
-- g) b = a + 1 -> nothing is printed
-- h) a + b + (a * b) -> 19
-- i) a == b -> False
-- j) if (b > a) && (b < a*b)
--    then b
--    else a
--    -> b -> 4
-- k)   a == 4 = 6 
--    | b == 4 = 6 + 7 + a
--    | otherwise = 25
--    -> 16
-- l) 2 + (if b > a then b else a) -> 6
-- m) (a > b = a | a < b = b | otherwise = (-1)) * (a + 1) -> 16
--
-- Exercise 1.2) Translate the following expression into prefix form
-- (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (-2 7)))
--
-- Exercise 1.3) Define a procedure that takes three numbers as arguments and
-- returns the sum of the squares of the two larger numbers
largeSquares x y z
    | x > z && y > z = sumOfSquares x y
    | y > x && z > x = sumOfSquares y z
    | x > y && z > y = sumOfSquares x z

-- Exercise 1.4) Describe the behavior of the following procedure:
-- aPlusAbsB a b = (if b > 0 then (+) else (-)) a b
-- this procedure returns another procedure (either plus or minus) and applies it to a and b
--
-- Exercise 1.5) Ben Bitdiddle has invented a test to determine whether the
-- interpreter he is faced with is using applicative-order evaluation or
-- normal-order evaluation. He defines the following to procedures:
  
-- f = f
-- test x y = if x == 0 then 0 else y
  
-- Then he evaluates the expression: test 0 f
-- Applicative-order - evaluates the body of test without evaluating f resulting
-- in 0
-- Normal-order - evaluates the body of f and infinitely stills in a loop 

sqrtIter guess x = if goodEnough guess x then guess else sqrtIter (improve guess x) x 

improve guess x = average guess (x / guess)

average x y = (x + y) / 2

goodEnough guess x = (abs ((square guess) - x)) < 0.001

sqrt' x = sqrtIter 1.0 x

-- Exercise 1.6) Alyssa P. Hacker doesn't see why if needs to be provided as a
-- special form. "Why can't I just define it as an ordinary procedure in terms
-- of cond?" she asks. Alyssa's friend Eva Lu Ator claims that this can indeed
-- be done, and she defines a new version of if.
  
newIf predicate thenClause elseClause
    | predicate = thenClause
    | otherwise = elseClause

sqrtIter' guess x = newIf (goodEnough guess x) guess (sqrtIter' (improve guess x) x)

sqrt'' x = sqrtIter' 1.0 x
-- What happens when Alyssa attempts to use this to compute square roots?
-- Explain. Nothing??
--
-- Exercise 1.7) An alternative strategy for implementing goodEnough is to watch
-- how guess changes from one iteration to the next and to stop when the change
-- is a very small fraction of the guess. Design a square-root procedure that
-- uses this kind of end test. Does this work better for small and bigger
-- numbers?
{- goodEnough' guess pastGuess x
    | guess = (abs (guess - pastGuess)) < 0.001
    | otherwise = (abs ((square guess) - x)) < 0.001
-} 

-- Exercise 1.8) Newton's method for cube roots is based on the fact that if y
-- is an approximation to the cube root of x, then a better approximation is
-- given by: (x/y^2 + 2y)/3
-- Use this formula to implement the cube-root procedure analogous to the
-- square-root procedure
cubeIter guess x = if cubeIsGood guess x then guess else cubeIter (improve' guess x) x

improve' y x = (x / (y^2) + 2 * y) / 3

cubeIsGood guess x = (abs ((cube guess) - x)) < 0.001

cube x = x * x * x

cubert x = cubeIter 1.0 x

factorial n = if n == 1 then 1 else n * (factorial (n - 1))

factorial' n = factorIter 1 1 n

factorIter prod counter maxCount = if counter > maxCount then prod else factorIter (counter * prod) (counter + 1) maxCount

-- Exercise 1.9) The following two procedures defines a method for adding two
-- positive integers in terms ofthe procedures inc, which increments its
-- argument by 1, and dec, which decrements its argument by 1.
infixl 6 +|
infixl 6 |+

a +| b = if a == 0 then b else inc ((dec a) + b)
a |+ b = if a == 0 then b else inc (a + (dec b))
inc x = x + 1
dec x = x - 1
-- Are these proceses iterative or recursive?
-- they are iterative because they keep following the state of one of the
-- numbers, i.e. the procedure doesn't reference itself again it simply keeps
-- track of state.
  
-- Exercise 1.10) The following procedure computes a mathematical function
-- called Ackermann's function:
ackermann x y
    | y == 0 = 0
    | x == 0 = 2 * y
    | y == 1 = 2
    | otherwise = ackermann (x - 1) (ackermann x (y - 1))
-- what are the values of the following expressions?
-- ackermann 1 10 -> 2 WRONG 1024
-- ackermann 2 4 -> 2 WRONG 65536
-- ackermann 3 3 -> 2 WRONG 65536 
--
-- Consider the following procedures:
f n = ackermann 0 n
g n = ackermann 1 n
h n = ackermann 2 n
k n = 5 * n * n
-- Give concise mathematical definitions for the functions computed by the
-- procedures f, g, and h for positive integer n. For example, k n computes 5n^2
-- f n computes n * 2
-- g n computes n * 2 + 1 WRONG computes 2^n
-- h n computes 2^(k) where k = n^n, i.e. if k = 3 then k = n^n^n
 
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fib' n = fibIter 1 0 n
fibIter a b count = if count == 0 then b else fibIter (a+b) a (count - 1)

-- Exercise 1.11) A function f is defined by the rule that f(n) = n if n < 3 and
-- f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3. Write a procedure that computes
-- f by means  of a recursive process and one by means of an iterative one.
-- recursive
fRecursive n 
    | n < 3 = n
    | otherwise = fRecursive (n - 1) + (2 * fRecursive (n-2)) + (3 * fRecursive (n - 3)) 
-- iterative
fIterative n = fIterativeIter 2 1 0 n
fIterativeIter a b c count = if count < 3 then a else fIterativeIter (a+(2*b)+(3*c)) a b (count - 1)
-- Exercise 1.12) Recall Pascal's triangle. The edges (borders) of the triangle
-- are all 1, and each number inside is the sum of the two numbers above it.
-- Write a procedure that computes Pascal's triangle by means of a recursive
-- process and one by means of an iterative one.
pascalRecursive n = undefined
pascalIterative n = undefined
-- Exercise 1.13) Prove that Fib(n) is the closes integer to phi^2 / sqrt(5),
-- where phi = (1+sqrt(5)) / 2. Hint: Let psi = (1 - sqrt(5)) / 2. Use induction
-- and the definition of the Fibonacci numbers to prove that 
-- Fib(n) = (phi^2 - psi^2) / sqrt(5).

