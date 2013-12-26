module SRuby where

main :: IO ()
main = do
    line <- getLine
    let result = interpret line
    print result

interpret = id


data Method = M

pMethod :: Parser Method

