import Interpreter
import Types
import Parsing
import Importer

main = do putStrLn "Hasket 1.4"
          eventLoop defaultEnvironment getOutput

mainDebug = do putStrLn "Hasket 1.4"
               eventLoop defaultEnvironment getOutputDebug



pairPrint :: String -> String
pairPrint pair = case getVariableValue pairEnv "x" of (Right x) -> case getVariableValue pairEnv "y" of (Right y) -> ('(':(printer x)) ++ (' ':(printer y)) ++ ")"
    where pieces = getExps $ chopParentheses pair
          pairEnv = (read $ pieces !! 1) :: Environment

funcPrint :: String -> String
funcPrint output = case getVariableValue funcEnv "INTERPRETER_PAIR" of (Right _) -> pairPrint output
                                                                       (Left _) -> "<#lambda>"
    where pieces = getExps $ chopParentheses output
          funcEnv = (read $ pieces !! 1) :: Environment

printer :: String -> String
printer output
  | take 15 output == "(evaledFunction" = funcPrint output
  | otherwise = output

getOutput :: Evaluated -> String
getOutput (Right (x,y)) = "> " ++ (printer x)
getOutput (Left x) = x

getOutputDebug :: Evaluated -> String
getOutputDebug (Right (x,y)) = "> " ++ x
getOutputDebug (Left x) = x

importPrinter :: (Evaluated -> String) -> ImportEvaluated -> [String]
importPrinter outputHandler (Right results) = map outputHandler results
importPrinter outputHandler (Left failure) = map outputHandler [failure]

eventLoop :: Environment -> (Evaluated -> String) -> IO ()
eventLoop env outputHandler = do exp <- getLine
                                 if exp == "quit" then putStrLn $ show env else do
                                    if take 6 exp == "import" then
                                       do fileText <- readFile $ drop 7 exp
                                          let delims = delimiterCheck fileText
                                          if (fst delims) then do
                                             let fileContents = drop 2 $ inputParser fileText
                                             let results = importEvaluatorLoop env fileContents
                                             let outputs = importPrinter outputHandler $ fst results
                                             sequence $ map putStrLn outputs
                                             eventLoop (snd results) outputHandler
                                          else do putStrLn $ outputHandler (Left $ snd delims)
                                                  eventLoop env outputHandler
                                       else do let delims = delimiterCheck exp
                                               let result = if not $ fst delims then Left $ snd delims else eval (exp,env)
                                               putStrLn $ outputHandler $ result
                                               case result of Right (x,y) -> eventLoop y outputHandler
                                                              Left _ -> eventLoop env outputHandler

























