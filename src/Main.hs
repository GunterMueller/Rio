{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import qualified Data.Map.Strict as Map

import System.Console.Haskeline
import System.Environment

import Syntax

import Typing.Errors
import Typing.Monad

import Main.Compile

toplevel :: Gamma
toplevel =
  Gamma . Map.fromList $
    [ (Var "add",   poly $ "Int" :-> "Int" :-> "Int")
    , (Var "if",    poly $ "Bool" :-> "a" :-> "a" :-> "a")
    , (Var "true",  poly "Bool")
    , (Var "false", poly "Bool")
    ]
  where poly = Typing Nothing mempty

main :: IO ()
main = do
  x <- getArgs
  case x of
    [file, out] -> compile file out
    [file] -> compile file "rio.out"
    _ -> repl

repl :: IO ()
repl = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "⊢ "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input -> do
        liftIO (interp input)
        loop

  interp :: String -> IO ()
  interp s =
    case realise toplevel s of
      Left e -> either print (putStrLn . showError s) e
      Right (_, ty) -> print ty 

