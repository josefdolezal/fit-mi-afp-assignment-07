module CalculatorModel where

type Calculation = Either String Double

data UnaryOperation = SQRT Double
                    | SIN Double
                    | COS Double
                    | ULOG Double
                    deriving(Show)

data BinaryOperation = Double `ADD` Double
                     | Double `SUB` Double
                     | Double `MUL` Double
                     | Double `DIV` Double
                     | Double `GCD` Double
                     | Double `POW` Double
                     | Double `BINLOG` Double
                     deriving(Show)

data Operation = Binary BinaryOperation
               | Unary UnaryOperation
               | Constant Double
               deriving(Show)
