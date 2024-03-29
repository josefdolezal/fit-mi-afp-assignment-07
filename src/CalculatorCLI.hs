module CalculatorCLI (calculator) where

import CalculatorModel
import Calculator

-- | Help for you and the users
help :: IO ()
help = do
  putStrLn "-------------------------"
  putStrLn "Binary operations:"
  putStrLn " ADD X Y"
  putStrLn " SUB X Y"
  putStrLn " MUL X Y"
  putStrLn " DIV X Y" -- Y /= 0
  putStrLn " GCD X Y" -- X > Y, positive
  putStrLn " POW X Y" -- X /= 0 || Y /= 0
  putStrLn " LOG B X" -- X > 0 && B > 0
  putStrLn "Unary functions:"
  putStrLn " SQRT X"  -- X >= 0
  putStrLn " LOG X"   -- B = 10, X > 0
  putStrLn " SIN X"
  putStrLn " COS X"
  putStrLn "Constants:" -- constant are just [A-Z]+ strings
  putStrLn " PI (-PI)"
  putStrLn "Others:"
  putStrLn " ?"
  putStrLn "Exit:"
  putStrLn " Ctrl + D"
  putStrLn "-------------------------"

-- | CLI for calculator
calculator :: IO ()
calculator = do
    putStrLn "Even more Basic Calculator (use '?' for help)"
    mapM_ cli . lines =<< getContents

cli :: String -> IO ()
cli "?" = help
cli r   = printResult $ calculate r

printResult :: Either String Double -> IO ()
printResult (Left e)  = putStrLn $ "Error: " ++ e
printResult (Right v) = putStrLn $ "Result: " ++ (show v)
