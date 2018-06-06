module Main where

import HLisp.Data
import HLisp.Parser
import HLisp.Eval (evalProgram)
import HLisp.Stdlib (stdlib)

main :: IO ()
main = do
  contents <- getContents
  case parse contents of
    Left err      -> putStrLn $ show err
    Right program -> case evalProgram stdlib program of
      Left err  -> putStrLn $ "There was an error: " ++ err
      Right res -> putStrLn $ show res

