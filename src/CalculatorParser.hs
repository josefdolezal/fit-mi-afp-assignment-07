module CalculatorParser (parseOperation) where

import Text.ParserCombinators.Parsec

import CalculatorModel

parseOperation :: String -> Either String Operation
parseOperation s = case (parse operation "Error" s) of
    Left _ -> Left "Error"
    Right a -> Right a

operation :: GenParser Char st Operation
operation = try
    (Binary <$> binaryOperation) <|>
    (Unary <$> unaryOperation)

unaryOperation :: GenParser Char st UnaryOperation
unaryOperation = try
    (unop "SQRT" SQRT) <|>
    (unop "LOG" ULOG)  <|>
    (unop "SIN" SIN)   <|>
    (unop "COS" COS)

binaryOperation :: GenParser Char st BinaryOperation
binaryOperation = try
    (binop "ADD" ADD) <|>
    (binop "SUB" SUB) <|>
    (binop "MUL" MUL) <|>
    (binop "DIV" DIV) <|>
    (binop "GCD" GCD) <|>
    (binop "POW" POW) <|>
    (binop "LOG" BINLOG)

unop :: String -> (Double -> UnaryOperation) -> GenParser Char st UnaryOperation
unop op f = do
    string op
    spaces
    f <$> operand

binop :: String -> (Double -> Double -> BinaryOperation) -> GenParser Char st BinaryOperation
binop op f = do
    string op
    spaces
    leftOp <- operand
    spaces
    rightOp <- operand
    return $ f leftOp rightOp

operand :: GenParser Char st Double
operand = try
    expr <|>
    neg expr

expr :: GenParser Char st Double
expr = try
    constant <|>
    number

neg :: GenParser Char st Double -> GenParser Char st Double
neg ex = try
    (parensNeg ex) <|>
    (plainNeg ex)

parensNeg :: GenParser Char st Double -> GenParser Char st Double
parensNeg ex = parens $ plainNeg ex

plainNeg :: GenParser Char st Double -> GenParser Char st Double
plainNeg ex = do { char '-'; (*(-1)) <$> ex }

parens :: GenParser Char st a -> GenParser Char st a
parens = between (char '(') (char ')')

number :: GenParser Char st Double
number = do { digits <- many1 digit; return (read digits :: Double) }

constant :: GenParser Char st Double
constant = constPi

constPi :: GenParser Char st Double
constPi = do { string "PI"; return pi }
