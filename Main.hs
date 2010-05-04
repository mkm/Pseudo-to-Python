module Main where

import Prelude hiding (either)
import System
import Control.Monad

main = getArgs >>= mapM_ (\fileName -> readFile fileName >>= writeFile (fileName ++ ".py") . compile)

compile = unlines . cstyler [0] . lines

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

satisfies pred = Parser $ \str -> case str of
    [] -> Left UnknownError
    (x:xs) -> if pred x then Right (x, xs) else Left UnknownError

zeroOrMore p = oneOrMore p <|> return []

oneOrMore p = return (:) `ap` p `ap` zeroOrMore p

oneOf xs = satisfies (`elem` xs)

either = foldr (<|>) failure

expect = satisfies . (==)

between l h = satisfies (\x -> l <= x && x <= h)

letter = either [between 'a' 'z', between 'A' 'Z', oneOf "-_"]

word = oneOrMore letter

arithOp = oneOf "+-*/%"

