module Importer (fileReader, inputParser, importEvaluatorLoop) where

import Parsing
import Interpreter
import Types

main = do action <- readFile "testprogram.rkt"
          putStrLn action



fileReader :: IO [String]
fileReader = do fileName <- getLine
                fileContents <- readFile fileName
                return $ drop 2 $ inputParser fileContents


inputParser :: String -> [String]
inputParser inputFile = getExps $ newlineRemover inputFile

newlineHelper :: String -> Bool -> String
newlineHelper [] _ = []
newlineHelper (x:xs) True
  | x == '\n' || x == ' ' = newlineHelper xs True
  | otherwise = ' ':x:(newlineHelper xs False)
newlineHelper (x:xs) False
  | x == '\n' = newlineHelper xs True
  | otherwise = x:(newlineHelper xs False)

newlineRemover :: String -> String
newlineRemover str = newlineHelper str False

{-
importEvaluatorLoop :: Environment -> [String] -> ([Evaluated], Environment)
importEvaluatorLoop env [] = ([], env)
importEvaluatorLoop env (x:xs) = let delims = delimiterCheck x
                                     result = if not $ fst delims then Left $ snd delims else eval (x,env)
                                     next = case result of Right (x,y) -> importEvaluatorLoop y xs
                                                           Left _ -> importEvaluatorLoop env xs
                                 in (result:(fst next),snd next)
-}


importEvaluatorLoop :: Environment -> [String] -> (ImportEvaluated, Environment)
importEvaluatorLoop env [] = (Right [], env)
importEvaluatorLoop env (x:xs) = let delims = delimiterCheck x
                                     result = if not $ fst delims then Left $ snd delims else eval (x,env)
                                 in case result of Right (x,y) -> let next = importEvaluatorLoop y xs
                                                                  in (fst next >>= (\x -> Right $ result:x),snd next)
                                                   Left _ -> (Left result, env)
















