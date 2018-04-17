module Calculator (calculate) where

import CalculatorModel
import CalculatorParser

-- Use monads, and functors and/or applicative to make the code simple!

-- Devise own operations with errors, here are msgs for you
e0 x = "Unknown operation: " ++ show x
e1 x = "Unknown constant: " ++ show x
e2 x = "Not a number:" ++ show x
e3 g r = "Missing operand/s (" ++ show g ++ " given, " ++ show r ++ " required)"
e4   = "DIV - Y cannot be 0"
e5   = "GCD - X must be greater or equal to Y"
e6   = "GCD - X and Y must be natural numbers"
e7   = "POW - X and Y cannot be both 0"
e8   = "LOG - X and B must be greater than 0"
e9   = "LOG - B must be greater than 0"
e10  = "LOG - X must be greater than 0"
e11  = "SQRT - X must be greater or equal to 0"
e12  = "No input"

-- | Function do to the calculation
calculate :: String -> Calculation
calculate "" = Left e12
calculate i = parseOperation i >>= process

process :: Operation -> Calculation
process (Unary u)  = Right $ processUnary u
process (Binary b) = Right $ processBinary b

processUnary :: UnaryOperation -> Double
processUnary (SQRT a) = safeSqrt a
processUnary (SIN a)  = sin a
processUnary (COS a)  = cos a
processUnary (ULOG a) = safeLog 10 a

processBinary :: BinaryOperation -> Double
processBinary (ADD l r)    = l + r
processBinary (SUB l r)    = l - r
processBinary (MUL l r)    = l * r
processBinary (DIV l r)    = safeDiv l r
processBinary (GCD l r)    = safeGCD l r
processBinary (POW l r)    = safePow l r
processBinary (BINLOG l r) = safeLog l r

safeSqrt :: Double -> Double
safeSqrt a = 0 --fixme

safeDiv :: Double -> Double -> Double
safeDiv l r = 0 --fixme

safeGCD :: Double -> Double -> Double
safeGCD l r = 0 --fixme

safePow :: Double -> Double -> Double
safePow l r = 0 --fixme

safeLog :: Double -> Double -> Double
safeLog l r = logBase l r