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
x >~ y = not (x < y)

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

-- Exercise 1.5)
