main = eventLoop defaultEnvironment

type LocalEnvironment = [(String, String)]
type Environment = [LocalEnvironment]
type Evaluated = Either String (String, Environment)
type Primitive = Environment -> [String] -> Evaluated

chopParentheses :: String -> String
chopParentheses str = tail $ init $ str

unWords :: [String] -> String
unWords lis = tail $ foldl (\x y -> x ++ " " ++ y) [] lis

getOutput :: Evaluated -> String
getOutput (Right (x,y)) = "> " ++ x
getOutput (Left x) = x

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
  | q = (False,"Error: Unmatched quotation mark")
  | n > 0 = (False,"Error: Unmatched (")
  | n < 0 = (False,"Error: Unexpected )")
  | otherwise = (True,"All good")
  where (n,q) = delimCounter str 0 False

eventLoop :: Environment -> IO ()
eventLoop env = do exp <- getLine
                   let delims = delimiterCheck exp
                   let result = if not $ fst delims then Left $ snd delims else eval (exp,env)
                   putStrLn $ getOutput $ result
                   case result of Right (x,y) -> eventLoop y
                                  Left _ -> eventLoop env

getExpsHelper :: String -> Int -> String -> Bool -> [String]
getExpsHelper [] 0 curr False = [reverse curr] 
getExpsHelper [] 0 curr True = error "Unmatched quotation mark"
getExpsHelper [] n c _ = error $ "Unmatched parentheses " ++ (show c)
getExpsHelper (x:xs) n curr quote
  | x == ' ' && n == 0 && not quote = (reverse curr):(getExpsHelper xs 0 [] False)
  | x == '(' && n == 0 && not quote = getExpsHelper xs 1 "(" False
  | x == '(' && not quote = getExpsHelper xs (n + 1) (x:curr) False
  | x == ')' && not quote = getExpsHelper xs (n - 1) (x:curr) False
  | x == '\"' && quote = ('\"':(reverse curr)):(getExpsHelper (tail xs) n [] False)
  | x == '\"' = getExpsHelper xs n "\"" True
  | otherwise = getExpsHelper xs n (x:curr) quote

getExps :: String -> [String]
getExps exp = getExpsHelper exp 0 [] False

cons :: String
cons = "(lambda (x y) (lambda (m) (if (= m 0) x y)))"

car :: String
car = "(lambda (p) (p 0))"

cdr :: String
cdr = "(lambda (p) (p 1))"

defaultEnvironment :: Environment
defaultEnvironment = [[("#t","True"),
                       ("True","True"),
                       ("#f","False"),
                       ("False","False"),
                       ("cons",cons),
                       ("car",car),
                       ("cdr",cdr)]]

eval :: (String,Environment) -> Evaluated
eval (exp,env)
-- Number
  | not $ null $ ((reads [(head exp)]) :: [(Float, String)]) = Right (expHead, env)
  | head exp == '\"' = Right (exp,env)
  | (head exp) /= '(' = (getVariableValue env expHead) >>= (\x -> Right (x, env))
  | firstItem == "define" = executeDefinition env exp
  | firstItem == "if" = executeIf env exp
  | firstItem == "lambda" = Right (exp,env)
  | primitive firstItem = executePrimitive env firstItem $ tail $ getExps $ chopParentheses exp
  | otherwise = let pieces = getExps $ chopParentheses exp
                    args = tail pieces
                in applyFunction env (eval ((head pieces),env)) args
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


applyFunction :: Environment -> Evaluated -> [String] -> Evaluated
applyFunction env (Right (func,_)) args = let evaledArgs = map (eval . (\x -> (x,env))) args
                                              funcPieces = getExps $ chopParentheses func
                                              parameters = words $ chopParentheses $ funcPieces !! 1
                                              funcBody = tail $ foldl (\x y -> x ++ " " ++ y) [] (drop 2 funcPieces)
                                              newLocalEnvironment = zip parameters evaledArgs
                                              in concatEvaledArgs evaledArgs >>= (\y -> (eval (funcBody,(zip parameters y):env))) >>= (\(r,(x:xs)) -> Right (r,xs))
applyFunction _ (Left str) _ = Left str


makeFunction :: String -> Environment -> String
makeFunction str env
  | take 7 str /= "(lambda" = error "Bad function definition"
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

executeFunctionDefinition :: Environment -> [String] -> Evaluated
executeFunctionDefinition env items = result
  where args = words $ chopParentheses $ items !! 1
        lambda = "(lambda (" ++ (unwords $ tail args) ++ ") " ++ (unWords $ drop 2 items) ++ ")"
        result = addBinding env (head args) $ lambda

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
              ("=",primitiveNumEq)]

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
primitiveSub env (x:y:[]) = Right (show $ x2 - y2,env)
    where x2 = read x
          y2 = read y
primitiveSub _ xs = Left "Error: Too many arguments: -"

primitiveDiv :: Primitive
primitiveDiv _ [] = Left "Error: Not enough arguments: /"
primitiveDiv env (x:y:[]) = Right (show $ x2 / y2,env)
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

primitiveOr :: Primitive
primitiveOr _ [] = Left "Error: Not enough arguments: or"
primitiveOr env (x:[]) = Left "Error: Not enough arguments: or"
primitiveOr env lis = Right (show $ or $ map booleanMaker lis, env)

primitiveAnd :: Primitive
primitiveAnd _ [] = Left "Error: Not enough arguments: and"
primitiveAnd env (x:[]) = Left "Error: Not enough arguments: and"
primitiveAnd env lis = Right (show $ and $ map booleanMaker lis, env)







