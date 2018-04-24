module Calculator (calculate) where

import CalculatorModel
import CalculatorParser

-- Use monads, and functors and/or applicative to make the code simple!

-- Devise own operations with errors, here are msgs for you
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
process (Unary u)    = processUnary u
process (Binary b)   = processBinary b
process (Constant c) = Right c

processUnary :: UnaryOperation -> Calculation
processUnary (SQRT a) = safeSqrt a
processUnary (ULOG a) = safeLog 10 a
processUnary (SIN a)  = Right $ sin a
processUnary (COS a)  = Right $ cos a

processBinary :: BinaryOperation -> Calculation
processBinary (ADD l r)    = Right $ l + r
processBinary (SUB l r)    = Right $ l - r
processBinary (MUL l r)    = Right $ l * r
processBinary (DIV l r)    = safeDiv l r
processBinary (GCD l r)    = safeGCD l r
processBinary (POW l r)    = safePow l r
processBinary (BINLOG l r) = safeLog l r

safeSqrt :: Double -> Calculation
safeSqrt x
    | x >= 0    = Right $ sqrt x
    | otherwise = Left e11

safeDiv :: Double -> Double -> Calculation
safeDiv l r
    | r /= 0    = Right $ l / r
    | otherwise = Left e4

safeGCD :: Double -> Double -> Calculation
safeGCD l r
    | invalidGCDOperands = Left e6
    | l > r              = Right $ fromIntegral $ gcd (round l) (round r)
    | otherwise          = Left e5
        where invalidGCDOperands = l < 0 || r < 0 || (wasDecimal l) || (wasDecimal r)

safePow :: Double -> Double -> Calculation
safePow 0 0 = Left e7
safePow l r = Right $ l**r

safeLog :: Double -> Double -> Calculation
safeLog 0 0 = Left e8
safeLog 0 _ = Left e9
safeLog _ 0 = Left e10
safeLog l r = Right $ logBase l r

-- Not a really good solution, but everything is already double..
wasDecimal :: Double -> Bool
wasDecimal a = a - sanitized /= 0
    where sanitized = fromIntegral . round $ a
