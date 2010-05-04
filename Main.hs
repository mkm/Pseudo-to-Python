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

either = foldr (<|>) failure

expect = satisfies . (==)

match w = sequence $ map expect w

between l h = satisfies (\x -> l <= x && x <= h)

letter = either [between 'a' 'z', between 'A' 'Z', oneOf "-_"]

word = oneOrMore letter

digit = between '0' '9'

number = oneOrMore digit

arithOp = oneOf "+-*/%"

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
    | Plus Expression Expression
    | Minus Expression Expression
    | Times Expression Expression
    | Divide Expression Expression
    deriving Show

stmtToSource (ForLoop a b c) = "for " ++ exprToSource a ++ " in range(" ++ exprToSource b ++ ", (" ++ exprToSource c ++ ") - 1):"
stmtToSource (WhileLoop a) = "while " ++ exprToSource a ++ ":"
stmtToSource (FunctionDecl name params) = "def " ++ name ++ "(" ++ foldr1 (\x y -> x ++ ", " ++ y) params ++ "):"
stmtToSource (Assignment to from) = exprToSource to ++ " = " ++ exprToSource from
stmtToSource (ReturnStmt a) = "return " ++ exprToSource a
stmtToSource EmptyStmt = ""

exprToSource (Variable name) = name
exprToSource (Constant value) = value

sourceLine = do
    indent <- ws
    x <- statement <|> return EmptyStmt
    endOfSource
    return (indent, x)

statement = either [forLoop, whileLoop, functionDecl, assignment, returnStmt]

expression = either [variable, constant{-, plus, minus, times, divide-}]

forLoop = do
    ws
    match "for"
    ws
    loopVar <- variable
    ws
    match "="
    ws
    initExpr <- expression
    ws
    match "to"
    ws
    termExpr <- expression
    return $ ForLoop loopVar initExpr termExpr

whileLoop = do
    ws
    match "while"
    ws
    termExpr <- expression
    return $ WhileLoop termExpr

functionDecl = do
    ws
    name <- word
    match "("
    params <- (word `seperatedBy` match ", ") <|> return []
    match ")"
    return $ FunctionDecl name params

assignment = do
    ws
    var <- variable
    ws
    match "="
    ws
    expr <- expression
    return $ Assignment var expr

returnStmt = do
    ws
    match "return"
    ws
    expr <- expression
    return $ ReturnStmt expr

variable = do
    w <- word
    return $ Variable w

constant = do
    n <- number
    return $ Constant n



