{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main.Compile (compile, realise) where

import qualified Data.Map.Strict as Map

import System.Console.Haskeline
import System.Directory
import System.Process
import System.Exit
import System.IO

import Parser
import Syntax

import Typing.Errors
import Typing.Monad
import Typing

import Mach.Assemble
import Core.Lower
import Gm.Compile

import Paths_riolml

realise :: Gamma -> String -> Either (Either ParseError TypeError) (Exp, Typing)
realise toplevel code = do
  expr <- mapLeft Left (parser code)
  mapLeft Right $
    fmap (expr,) (runTyping toplevel (infer expr))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

compile :: String -> String -> IO ()
compile file out_path = do
  runtime <- getDataFileName "rts/rts.o"
  source <- readFile file
  (code, Typing _ (Delta env) ty) <-
    case realise mempty source of
      Left e -> do
        either print (putStrLn . showError source) e
        exitFailure
      Right x -> pure x

  when (not (null env)) $ do
    _ <- traverse putStrLn $
      [ "Program did not have closed typing."
      , "Free variables:"
      ] ++ map (\(x, t) -> "->  " ++ show x ++ " : " ++ show t) (Map.toList env)
    exitFailure

  when (not (isTyCon ty)) $ do
    _ <- traverse putStrLn $
      [ "Program did not result in a concrete, non-functional type."
      , "Result type: " ++ show ty
      ]
    exitFailure

  let core = lowerProg code
  let gm_code = map compileSc core
  let sects = assembleProg gm_code

  withTempFile "/tmp/" "rio-supercombinators.s" $ \asm_out handle -> do
    _ <- traverse (hPutStr handle . show) sects
    hPutStrLn handle ".globl _main"
    hFlush handle

    let desc =
          (proc "gcc" [ "-static", asm_out, runtime, "-o", out_path, "-no-pie" ])
            { std_out = NoStream
            , std_in = NoStream
            }
    withCreateProcess desc $ \_ _ _ handle -> exitWith =<< waitForProcess handle

withTempFile :: FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile path template cont =
  bracket (openTempFile path template) (\(x, y) -> hClose y *> removePathForcibly x) (uncurry cont)
