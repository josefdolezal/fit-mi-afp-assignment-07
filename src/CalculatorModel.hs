module CalculatorModel where

type Calculation = Either String Double

data UnaryOperation = SQRT Integer
                    | SIN Integer
                    | COS Integer
                    -- | LOG Integer -- Interpreted as binary

data BinaryOperation = Integer `ADD` Integer
                     | Integer `SUB` Integer
                     | Integer `MUL` Integer
                     | Integer `DIV` Integer
                     | Integer `GCD` Integer
                     | Integer `POW` Integer
                     | Integer `LOG` Integer

data Operation = BinaryOperation | UnaryOperation | Integer