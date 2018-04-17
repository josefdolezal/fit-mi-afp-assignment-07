module Calculator (calculate) where

import CalculatorModel

process :: Operation -> Calculation
process = undefined

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
calculate = undefined
