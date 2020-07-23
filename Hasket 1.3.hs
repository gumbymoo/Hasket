main = do putStrLn "Hasket 4.0"
          eventLoop defaultEnvironment

mainDebug = do putStrLn "Hasket 4.0"
               eventLoopDebug defaultEnvironment

type LocalEnvironment = [(String, String)]
type Environment = [LocalEnvironment]
type AllEnvironments = (Int,[(Int, Environment)])
type Evaluated = Either String (String, Environment)
type Primitive = Environment -> [String] -> Evaluated

chopParentheses :: String -> String
chopParentheses str = tail $ init $ str

unWords :: [String] -> String
unWords lis = tail $ foldl (\x y -> x ++ " " ++ y) [] lis

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

delimCounter :: String -> Int -> Bool -> (Int,Bool)
delimCounter [] n quote = (n,quote)
delimCounter (x:xs) n True
  | x == '\"' = delimCounter xs n False
  | otherwise = delimCounter xs n True
delimCounter (x:xs) n False
  | n < 0 = (n,False)
  | x == '(' = delimCounter xs (n + 1) False
  | x == ')' = delimCounter xs (n - 1) False
  | x == '\"' = delimCounter xs n True
  | otherwise = delimCounter xs n False

delimiterCheck :: String -> (Bool,String)
delimiterCheck str
  | str == "quit" = error "Goodbye"
  | q = (False,"Error: Unmatched quotation mark")
  | n > 0 = (False,"Error: Unmatched (")
  | n < 0 = (False,"Error: Unexpected )")
  | otherwise = (True,"All good")
  where (n,q) = delimCounter str 0 False

eventLoop :: Environment -> IO ()
eventLoop env = do exp <- getLine
                   if exp == "quit" then putStrLn $ show env else do
                   let delims = delimiterCheck exp
                   let result = if not $ fst delims then Left $ snd delims else eval (exp,env)
                   putStrLn $ getOutput $ result
                   case result of Right (x,y) -> eventLoop y
                                  Left _ -> eventLoop env

eventLoopDebug :: Environment -> IO ()
eventLoopDebug env = do exp <- getLine
                        if exp == "quit" then putStrLn $ show env else do
                        let delims = delimiterCheck exp
                        let result = if not $ fst delims then Left $ snd delims else eval (exp,env)
                        putStrLn $ getOutputDebug $ result
                        case result of Right (x,y) -> eventLoopDebug y
                                       Left _ -> eventLoopDebug env

getExpsHelper :: String -> Int -> String -> Bool -> [String]
getExpsHelper [] 0 curr False = [reverse curr] 
getExpsHelper [] 0 curr True = error "Unmatched quotation mark"
getExpsHelper [] n c _ = error $ "Unmatched parentheses " ++ (show c)
getExpsHelper (x:xs) n curr quote
  | x == ' ' && n == 0 && not quote = (reverse curr):(getExpsHelper xs 0 [] False)
  | (x == '(' || x == '[') && n == 0 && not quote = getExpsHelper xs 1 [x] False
  | (x == '(' || x == '[') && not quote = getExpsHelper xs (n + 1) (x:curr) False
  | (x == ')' || x == ']') && not quote = getExpsHelper xs (n - 1) (x:curr) False
  | x == '\"' && quote = (getExpsHelper xs n ('\"':curr) False)
  | x == '\"' = getExpsHelper xs n ('\"':curr) True
  | otherwise = getExpsHelper xs n (x:curr) quote

getExps :: String -> [String]
getExps exp = getExpsHelper exp 0 [] False

cons :: String
cons = "(evaledFunction [[(\"INTERPRETER_PAIR\",\"INTERPRETER_PAIR\")]] (x y) (lambda (m) (if (= m 0) x y)))"

car :: String
car = "(evaledFunction [] (p) (p 0))"

cdr :: String
cdr = "(evaledFunction [] (p) (p 1))"

lispEmpty :: String
lispEmpty = "(evaledFunction [] (x) (eq? x empty))"

lispMap :: String
lispMap = "(evaledFunction [] (f lis) (if (empty? lis) empty (cons (f (car lis)) (map f (cdr lis)))))"

lispFoldr :: String
lispFoldr = "(evaledFunction [] (f init lis) (if (empty? lis) init (f (car lis) (foldr f init (cdr lis)))))"

list :: String
list = "(evaledFunction [] (. w) (foldr cons empty w))"

defaultEnvironment :: Environment
defaultEnvironment = [[("#t","True"),
                       ("True","True"),
                       ("#f","False"),
                       ("False","False"),
                       ("cons",cons),
                       ("car",car),
                       ("cdr",cdr),
                       ("empty","empty"),
                       ("empty?",lispEmpty),
                       ("map",lispMap),
                       ("foldr",lispFoldr),
                       ("list",list)]]

eval :: (String,Environment) -> Evaluated
eval (exp,env)
-- Number
  | not $ null $ ((reads [(head exp)]) :: [(Float, String)]) = Right (expHead, env)
  | head exp == '\"' = Right (exp,env)
  | (head exp) /= '(' && (head exp) /= '[' = (getVariableValue env expHead) >>= (\x -> Right (x, env))
  | firstItem == "define" = executeDefinition env exp
  | firstItem == "if" = executeIf env exp
  | firstItem == "lambda" = Right (makeFunction exp env,env)
  | primitive firstItem = executePrimitive env firstItem $ tail $ getExps $ chopParentheses exp
  | otherwise = let pieces = getExps $ chopParentheses exp
                    args = tail pieces
                in applyFunction2 env (eval ((head pieces),env)) args
  where subExps = words exp
        expHead = head subExps
        firstItem = tail expHead

getVariableValueFromEnvironment :: LocalEnvironment -> String -> String
getVariableValueFromEnvironment [] var = []
getVariableValueFromEnvironment ((name,val):xs) var
  | name == var = val
  | otherwise = getVariableValueFromEnvironment xs var

getVariableValue :: Environment -> String -> Either String String
getVariableValue [] var = Left $ "Error: Variable " ++ var ++ " is unbound"
getVariableValue (x:xs) var
  | null result = getVariableValue xs var
  | otherwise = Right result
  where result = getVariableValueFromEnvironment x var

addBinding :: Environment -> String -> String -> Evaluated
addBinding [] var val = Right ("Done", [[(var,val)]])
addBinding (x:xs) var val = if null $ filter (\(x,y) -> x == var) x
                            then Right ("Done", ((var,val):x):xs)
                            else Left $ "Error: Duplicate definition: " ++ var



concatEvaledArgs :: [Evaluated] -> Either String [String]
concatEvaledArgs [] = Right []
concatEvaledArgs (x:xs) = do (a,b) <- x
                             done <- concatEvaledArgs xs
                             return (a:done)

{-
applyFunction :: Environment -> Evaluated -> [String] -> Evaluated
applyFunction env (Right (func,_)) args = let evaledArgs = map (eval . (\x -> (x,env))) args
                                              funcPieces = getExps $ chopParentheses func
                                              parameters = words $ chopParentheses $ funcPieces !! 1
                                              funcBody = tail $ foldl (\x y -> x ++ " " ++ y) [] (drop 2 funcPieces)
                                              newLocalEnvironment = zip parameters evaledArgs --NOT USED
                                              in concatEvaledArgs evaledArgs >>= (\y -> (eval (funcBody,(zip parameters y):env))) >>= (\(r,(x:xs)) -> Right (r,xs))
applyFunction _ (Left str) _ = Left str

Outdated
-}

envRead :: String -> Environment
envRead str = if null result then error str else fst $ head $ result
  where result = reads str

dotListFunction :: String -> Bool
dotListFunction func = let pieces = getExps $ chopParentheses func
                       in (pieces !! 2) !! 1 == '.'

applyFunction2 :: Environment -> Evaluated -> [String] -> Evaluated
applyFunction2 env (Right (func,_)) args = if dotListFunction func
                                           then applyDotFunc env func args
                                           else let evaledArgs = map (eval . (\x -> (x,env))) args
                                                    funcPieces = getExps $ chopParentheses func
                                                    parameters = words $ chopParentheses $ funcPieces !! 2
                                                    funcBody = tail $ foldl (\x y -> x ++ " " ++ y) [] (drop 3 funcPieces)
                                                    funcEnvironment = (envRead $ funcPieces !! 1) :: Environment
                                                    in concatEvaledArgs evaledArgs >>= (\y -> (eval (funcBody,(zip parameters y):(funcEnvironment ++ env)))) >>= (\(r,(x:xs)) -> Right (r,env))
applyFunction2 _ (Left str) _ = Left str

consUp :: [String] -> String
consUp [] = "empty"
consUp (x:xs) = (("(cons " ++ x ++ " ") ++ (consUp xs)) ++ ")"

addSpaces :: [String] -> [String]
addSpaces [] = []
addSpaces (x:xs) = x:" ":(addSpaces xs)

dotFuncToFunc :: String -> String
dotFuncToFunc func = let pieces = getExps $ chopParentheses func
                         newArgs = '(':(drop 3 $ pieces !! 2)
                     in foldl (++) [] $ addSpaces $ (take 2 pieces) ++ [newArgs] ++ (drop 3 pieces)

applyDotFunc :: Environment -> String -> [String] -> Evaluated
applyDotFunc env func args = applyFunction2 env (Right (newFunc,env)) [lis]
  where lis = consUp args
        newFunc = init ('(':(dotFuncToFunc func)) ++ ")"


makeFunction :: String -> Environment -> String
makeFunction str env
  | take 7 str /= "(lambda" = error "Bad function definition -- if you're seeing me, you goofed hard"
  | otherwise = "(evaledFunction " ++ (show env) ++ (drop 7 str)


executeDefinition :: Environment -> String -> Evaluated
executeDefinition env exp = let pieces = getExps $ tail $ init exp
                            in if (head $ head $ tail pieces) == '('
                               then executeFunctionDefinition env pieces
                               else executeVariableDefinition env pieces

executeVariableDefinition :: Environment -> [String] -> Evaluated
executeVariableDefinition env items = do value <- eval ((items !! 2),env)
                                         result <- addBinding env (items !! 1) (fst value)
                                         return result

combineEnvironments :: Environment -> Environment -> Environment
combineEnvironments funcEnv workingEnv = funcEnv ++ workingEnv

executeFunctionDefinition :: Environment -> [String] -> Evaluated
executeFunctionDefinition env items = result
  where args = words $ chopParentheses $ items !! 1
        lambda = "(lambda (" ++ (unwords $ tail args) ++ ") " ++ (unWords $ drop 2 items) ++ ")"
        result = addBinding env (head args) $ makeFunction lambda env

booleanMaker :: String -> Bool
booleanMaker str
  | str == "False" = False
  | otherwise = True

executeIf :: Environment -> String -> Evaluated
executeIf env exp = condition >>= (\(c,_) -> if booleanMaker c
                                         then eval (exps !! 2,env)
                                         else if length exps < 4
                                              then Left "Error: Missing alternative: if"
                                              else eval (exps !! 3,env))
    where exps = getExps $ chopParentheses exp
          condition = eval (exps !! 1,env)


executePrimitive :: Environment -> String -> [String] -> Evaluated
executePrimitive env prim args = let evaledArgs = map (eval . (\x -> (x,env))) args
                                 in concatEvaledArgs evaledArgs >>= (lookupPrimitive prim env)

primitives :: [(String,Primitive)]
primitives = [("+",primitiveAdd),
              ("*",primitiveMul),
              ("-",primitiveSub),
              ("/",primitiveDiv),
              ("or",primitiveOr),
              ("and",primitiveAnd),
              ("=",primitiveNumEq),
              ("eq?",primitiveEq)]

primitive :: String -> Bool
primitive x = elem x $ map fst primitives

lookupPrimitive :: String -> Primitive
lookupPrimitive x = snd $ head $ filter (\(a,b) -> a == x) primitives


primitiveAdd :: Primitive
primitiveAdd _ [] = Left "Error: Not enough arguments: +"
primitiveAdd env lis = Right (show $ sum $ map read lis,env)

primitiveMul :: Primitive
primitiveMul _ [] = Left "Error: Not enough arguments: *"
primitiveMul env lis = Right (show $ product $ map read lis,env)

primitiveSub :: Primitive
primitiveSub _ [] = Left "Error: Not enough arguments: -"
primitiveSub env (x:[]) = Right (show $ 0 - (read x),env)
primitiveSub env (x:y:[]) = Right (show $ x2 - y2,env)
    where x2 = read x
          y2 = read y
primitiveSub _ xs = Left "Error: Too many arguments: -"

primitiveDiv :: Primitive
primitiveDiv _ [] = Left "Error: Not enough arguments: /"
primitiveDiv env (x:y:[])
  | y2 == 0 = Left "Error: Divide by 0"
  | otherwise = Right (show $ x2 / y2,env)
    where x2 = read x
          y2 = read y
primitiveDiv _ xs = Left "Error: Too many arguments: /"

readsNumber :: (Num a, Read a) => String -> [(a,String)]
readsNumber x = if null result then [] else result
    where result = reads x

primitiveNumEq :: Primitive
primitiveNumEq _ [] = Left "Error: Not enough arguments: ="
primitiveNumEq _ (x:[]) = Left "Error: Not enough arguments: ="
primitiveNumEq env (x:y:[]) = numbers
    where x' = readsNumber x
          y' = readsNumber y
          numbers = if not $ null x' || null y'
                    then Right (show $ (fst $ head $ x') == (fst $ head $ y'),env)
                    else Left "Error: Non-number argument: ="

primitiveEq :: Primitive
primitiveEq _ [] = Left "Error: Not enough arguments: eq"
primitiveEq _ (x:[]) = Left "Error: Not enough arguments: eq"
primitiveEq env (x:y:[]) = Right (show $ x == y,env)

primitiveOr :: Primitive
primitiveOr _ [] = Left "Error: Not enough arguments: or"
primitiveOr env (x:[]) = Left "Error: Not enough arguments: or"
primitiveOr env lis = Right (show $ or $ map booleanMaker lis, env)

primitiveAnd :: Primitive
primitiveAnd _ [] = Left "Error: Not enough arguments: and"
primitiveAnd env (x:[]) = Left "Error: Not enough arguments: and"
primitiveAnd env lis = Right (show $ and $ map booleanMaker lis, env)







