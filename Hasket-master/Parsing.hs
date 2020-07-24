module Parsing (chopParentheses, unWords, delimCounter, delimiterCheck, getExps) where

chopParentheses :: String -> String
chopParentheses str = tail $ init $ str

unWords :: [String] -> String
unWords lis = tail $ foldl (\x y -> x ++ " " ++ y) [] lis

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