module Main where

import Parser
import Solver

import System.Environment
import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)
import Data.List (intercalate)

logmsg = hPutStrLn stderr
output = putStrLn

main = do
  args <- getArgs
  input <- parseFile (args !! 0)
  case input of
    Left err -> do
      logmsg $ "Parse error: " ++ err
      exitFailure
    Right problem -> do
      solution <- plan logmsg 1000 problem
      case solution of
        Just plan -> do
          sequence_ [ output (formatItem item) | item <- plan ]
          exitSuccess
        Nothing -> exitFailure
      
formatItem (pred, args) = pred ++ if length args == 0 then "" else "(" ++ (intercalate "," args) ++ ")"
