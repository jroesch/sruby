module SRuby where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((*>), (<*), (<*>), (<$>))

main :: IO ()
main = do
    line <- getLine
    result <- interpret line
    print result

interpret input = case runParser pMethod () "REPL" input of
    Left e  -> print e
    Right v -> print v

interpretWith p input = case runParser p () "REPL" input of
    Left e  -> print e
    Right v -> print v


data Method = M deriving (Show, Eq)

hSpace :: Parser ()
hSpace = skipMany $ satisfy (\c -> (c == '\t') || (c == ' '))

pMethod :: Parser Method
pMethod = do
    string "def" <* hSpace
    name <- methodName
    char '(' <* hSpace
    -- args
    char ')' <* hSpace
    -- body
    return M

methodName :: Parser String
methodName = do
        x  <- first
        xs <- many rest
        return (x:xs)
    where first = letter <|> char '_'
          rest  = first  <|> digit


