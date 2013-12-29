module SRuby.Util.Printer where

type RubyPrinter a = S.State PrinterState a

data PrinterState = PrinterState Int String

indentationLevel :: PrinterState -> Int
indentationLevel (PrinterState i _) = i

output :: String -> RubyPrinter ()
output v = modify $ \(PrinterState i o) -> PrinterState i (o ++ v)

beginDef :: RubyPrinter ()
beginDef = 
    modify $ \(PrinterState i o) ->
        PrinterState (i + 1) (o ++ "def ")

endDef :: RubyPrinter ()
endDef = 
    modify $ \(PrinterState i o) ->
        PrinterState (i + 1) (o ++ "end\n")

class ToRuby a where
    toRuby :: a -> RubyPrinter ()

instance ToRuby Method where
    toRuby (Method name params body) = do
        beginDef
        output (" " ++ name)
        output $ let m = intercalate ", " (map show params) in concat ["(", m, ")\n"]
        output $ "\n"
        endDef

outputRuby :: (ToRuby a) => a -> String
outputRuby v = let PrinterState _ s = snd $ runState (toRuby v) (PrinterState 0 "")
                   in s

