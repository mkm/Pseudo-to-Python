module Main where

import Prelude hiding (either)
import System
import Control.Monad
import Data.List

main = getArgs >>= mapM_ (\fileName -> readFile fileName >>= writeFile (fileName ++ ".py") . compile)

compile = unlines . map compileLine . lines

compileLine input = case parse sourceLine input of
    Left err -> err
    Right (indent, str) -> indent ++ stmtToSource str

alternateWith _ [] = []
alternateWith seperator xs = foldr1 (\x y -> x ++ seperator ++ y) xs

data Error
    = UnknownError
    | EitherError Error Error
    deriving Show

data Parser a = Parser { runParser :: String -> Either Error (a, String) }

instance Monad Parser where
    p >>= f = Parser $ \str -> case runParser p str of
        Left err -> Left err
        Right (x, str') -> runParser (f x) str'
    return x = Parser $ curry Right x
    fail _ = failure

failure = Parser $ \_ -> Left UnknownError

parse p input = case runParser p input of
    Left err -> Left (show err)
    Right (x, _) -> Right x

infixl 3 <|>
p <|> q = Parser $ \str -> case runParser p str of
    Left err -> case runParser q str of
        Left err' -> Left (EitherError err err')
        Right x -> Right x
    Right x -> Right x

satisfies pred = Parser $ \str -> case str of
    [] -> Left UnknownError
    (x:xs) -> if pred x then Right (x, xs) else Left UnknownError

notFollowedBy p = Parser $ \str -> case runParser p str of
    Left err -> Right ((), str)
    Right _ -> Left UnknownError

anything = satisfies (const True)

zeroOrMore p = oneOrMore p <|> return []

oneOrMore p = return (:) `ap` p `ap` zeroOrMore p

oneOf xs = satisfies (`elem` xs)

seperatedBy p q = do
    head <- p
    tail <- zeroOrMore (q >> p)
    return $ head : tail

token p = ws >> p

either = foldr (<|>) failure

expect = satisfies . (==)

match w = token $ sequence $ map expect w

between l h = satisfies (\x -> l <= x && x <= h)

letter = either [between 'a' 'z', between 'A' 'Z', oneOf "-_"]

word = token $ oneOrMore letter

digit = between '0' '9'

number = token $ oneOrMore digit

arithOp = token $ oneOf "+-*/%"

white = oneOf " \n\t"

ws = zeroOrMore $ white

wss = oneOrMore $ white

endOfSource = notFollowedBy anything

data Statement
    = ForLoop Expression Expression Expression
    | WhileLoop Expression
    | FunctionDecl String [String]
    | Assignment Expression Expression
    | ReturnStmt Expression
    | EmptyStmt
    deriving Show

data Expression
    = Variable String
    | Constant String
    | Times [Expression]
    | Divide [Expression]
    | Plus [Expression]
    | Minus [Expression]
    deriving Show

stmtToSource (ForLoop a b c) = "for " ++ exprToSource a ++ " in range(" ++ exprToSource b ++ ", (" ++ exprToSource c ++ ") + 1):"
stmtToSource (WhileLoop a) = "while " ++ exprToSource a ++ ":"
stmtToSource (FunctionDecl name params) = "def " ++ name ++ "(" ++ alternateWith ", " params ++ "):"
stmtToSource (Assignment to from) = exprToSource to ++ " = " ++ exprToSource from
stmtToSource (ReturnStmt a) = "return " ++ exprToSource a
stmtToSource EmptyStmt = ""

exprToSource (Variable name) = name
exprToSource (Constant value) = value
exprToSource (Times exprs) = alternateWith " * " (map exprToSource exprs)
exprToSource (Divide exprs) = alternateWith " / " (map exprToSource exprs)
exprToSource (Plus exprs) = alternateWith " + " (map exprToSource exprs)
exprToSource (Minus exprs) = alternateWith " - " (map exprToSource exprs)

sourceLine = do
    indent <- ws
    x <- statement <|> return EmptyStmt
    endOfSource
    return (indent, x)

statement = either [forLoop, whileLoop, functionDecl, assignment, returnStmt]

expression = times

primitiveExpression = either [variable, constant]

times = return Times `ap` (divide `seperatedBy` match "*")
divide = return Divide `ap` (plus `seperatedBy` match "/")
plus = return Plus `ap` (minus `seperatedBy` match "+")
minus = return Minus `ap` (primitiveExpression `seperatedBy` match "-")

forLoop = do
    match "for"
    loopVar <- variable
    match "="
    initExpr <- expression
    match "to"
    termExpr <- expression
    return $ ForLoop loopVar initExpr termExpr

whileLoop = do
    match "while"
    termExpr <- expression
    return $ WhileLoop termExpr

functionDecl = do
    name <- word
    match "("
    params <- (word `seperatedBy` match ", ") <|> return []
    match ")"
    return $ FunctionDecl name params

assignment = do
    var <- variable
    match "="
    expr <- expression
    return $ Assignment var expr

returnStmt = do
    match "return"
    expr <- expression
    return $ ReturnStmt expr

variable = do
    w <- word
    return $ Variable w

constant = do
    n <- number
    return $ Constant n



