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

boolOp = either $ map match ["==", ">=", "<=", "<", ">"]

white = oneOf " \n\t"

ws = zeroOrMore $ white

wss = oneOrMore $ white

endOfSource = notFollowedBy anything

data Statement
    = ForLoop Expression Expression Expression
    | WhileLoop Expression
    | If Expression
    | Else
    | ElseIf Expression
    | FunctionCall [Expression]
    | FunctionDecl String [String]
    | Assignment Expression Expression
    | ReturnStmt Expression
    | EmptyStmt
    deriving Show

data Expression
    = Variable String
    | Constant String
    | Comparison String Expression Expression
    | Times [Expression]
    | Divide [Expression]
    | Plus [Expression]
    | Minus [Expression]
    | SubExpression Expression
    | FunctionInvocation String [Expression]
    | PropertyLookup Expression String
    deriving Show

stmtToSource (ForLoop a b c) = "for " ++ exprToSource a ++ " in range(" ++ exprToSource b ++ ", (" ++ exprToSource c ++ ") + 1):"
stmtToSource (WhileLoop a) = "while " ++ exprToSource a ++ ":"
stmtToSource (If a) = "if " ++ exprToSource a ++ ":"
stmtToSource Else = "else:"
stmtToSource (ElseIf a) = "elif " ++ exprToSource a ++ ":"
stmtToSource (FunctionDecl name params) = "def " ++ name ++ "(" ++ alternateWith ", " params ++ "):"
stmtToSource (Assignment to from) = exprToSource to ++ " = " ++ exprToSource from
stmtToSource (ReturnStmt a) = "return " ++ exprToSource a
stmtToSource EmptyStmt = ""

exprToSource (Variable name) = name
exprToSource (Constant value) = value
exprToSource (Comparison op a b) = exprToSource a ++ " " ++ op ++ " " ++ exprToSource b
exprToSource (Times exprs) = alternateWith " * " (map exprToSource exprs)
exprToSource (Divide exprs) = alternateWith " / " (map  exprToSource exprs)
exprToSource (Plus exprs) = alternateWith " + " (map exprToSource exprs)
exprToSource (Minus exprs) = alternateWith " - " (map exprToSource exprs)
exprToSource (SubExpression a) = parenthesise (exprToSource a)
exprToSource (FunctionInvocation name params) = name ++ "(" ++ alternateWith ", " (map exprToSource params) ++ ")"
exprToSource (PropertyLookup a name) = exprToSource a ++ "." ++ name

parenthesise str = "(" ++ str ++ ")"

sourceLine = do
    indent <- ws
    x <- statement <|> return EmptyStmt
    endOfSource
    return (indent, x)

statement = either [forLoop, whileLoop, ifStmt, elseStmt, elseifStmt, functionDecl, assignment, returnStmt]

expression = comparison <|> times

primitiveExpression = either [propertyLookup, variable, constant, subExpression]

paramList = (word `seperatedBy` match ", ") <|> return []

comparison = do
    left <- times
    op <- boolOp
    right <- times
    return $ Comparison op left right

times = return Times `ap` (divide `seperatedBy` match "*")
divide = return Divide `ap` (plus `seperatedBy` match "/")
plus = return Plus `ap` (minus `seperatedBy` match "+")
minus = return Minus `ap` ((functionInvocation <|> primitiveExpression) `seperatedBy` match "-")

functionInvocation = do
    func <- word
    match "("
    params <- (expression `seperatedBy` match ", ") <|> return []
    match ")"
    return $ FunctionInvocation func params

propertyLookup = do
    var <- variable
    match "."
    propName <- word
    return $ PropertyLookup var propName

subExpression = do
    match "("
    expr <- expression
    match ")"
    return $ SubExpression expr

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

ifStmt = do
    match "if"
    expr <- expression
    return $ If expr

elseStmt = do
    match "else"
    return Else

elseifStmt = do
    match "elseif"
    expr <- expression
    return $ ElseIf expr

functionDecl = do
    name <- word
    match "("
    params <- paramList
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



