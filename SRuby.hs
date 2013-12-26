module SRuby where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((*>), (<*), (<*>), (<$>))

main :: IO ()
main = do
    line <- getLine
    let result = interpret line
    print result

interpret = id

data Method = M

hSpace :: Parser ()
hSpace = satisfy (\c -> (c /= '\t') && (c /= ' ')) >> return ()

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
methodName = return "foobar"


