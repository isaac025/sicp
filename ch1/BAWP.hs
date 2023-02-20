module BAWP where

square x = x * x

sumOfSquares x y = square x + square y

f a = sumOfSquares (a + 1) (a * 2)

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
-- goodEnough' guess x
--
-- Exercise 1.8) Newton's method for cube roots is based on the fact that if y
-- is an approximation to the cube root of x, then a better approximation is
-- given by: (x/y^2 + 2y)/3
-- Use this formula to implement the cube-root procedure analogous to the
-- square-root procedure
