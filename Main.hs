module Main where

import Prelude hiding (either)
import System
import Control.Monad

main = getArgs >>= mapM_ (\fileName -> readFile fileName >>= writeFile (fileName ++ ".py") . compile)

compile = runParser sourceFile . unlines . cstyler [0] . lines

cstyler _ [] = []
cstyler indents@(curIndent:indentsRest) (x:xs) = case removeIndent x of
    (indentLevel, str) -> case compare indentLevel curIndent of
        LT -> if indentLevel == head indentsRest
            then "}" : (str ++ ";") : cstyler indentsRest xs
            else error "wrong indentation"
        EQ -> (str ++ ";") : cstyler indents xs
        GT -> "{" : (str ++ ";") : cstyler (indentLevel : indents) xs

removeIndent (' ':xs) = case removeIndent xs of
    (indentLevel, xs') -> (indentLevel + 1, xs')
removeIndent xs = (0, xs)

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

infixl 3 <|>
p <|> q = Parser $ \str -> case runParser p str of
    Left err -> case runParser q str of
        Left err' -> Left (EitherError err err')
        Right x -> Right x
    Right x -> Right x

satisfies pred = Parser $ \str -> case str of
    [] -> Left UnknownError
    (x:xs) -> if pred x then Right (x, xs) else Left UnknownError

zeroOrMore p = oneOrMore p <|> return []

oneOrMore p = return (:) `ap` p `ap` zeroOrMore p

oneOf xs = satisfies (`elem` xs)

seperatedBy p q = p >> zeroOrMore (q >> p)

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

data Statement
    = ForLoop Expression Expression Expression [Statement]
    | WhileLoop Expression [Statement]
    | FunctionDecl String [String] [Statement]
    | Assignment Expression Expression
    | ReturnStmt Expression
    deriving Show

data Expression
    = Variable String
    | Constant String
    | Plus Expression Expression
    | Minus Expression Expression
    | Times Expression Expression
    | Divide Expression Expression
    deriving Show

sourceFile = zeroOrMore functionDecl

statement = either [forLoop, whileLoop, functionDecl, assignment, returnStmt]

expression = either [variable, constant{-, plus, minus, times, divide-}]

block = do
    ws
    match "{"
    ws
    body <- zeroOrMore statement
    ws
    match "}"
    return body

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
    ws
    match ";"
    body <- block
    return $ ForLoop loopVar initExpr termExpr body

whileLoop = do
    ws
    match "while"
    ws
    termExpr <- expression
    ws
    match ";"
    body <- block
    return $ WhileLoop termExpr body

functionDecl = do
    ws
    name <- word
    match "("
    params <- (word `seperatedBy` match ", ") <|> return []
    match ")"
    ws
    match ";"
    body <- block
    return $ FunctionDecl name params body

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



