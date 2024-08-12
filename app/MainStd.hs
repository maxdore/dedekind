{-# LANGUAGE BangPatterns #-}

module Main where

import System.CPUTime
import qualified System.Timeout as T
import Options.Applicative

import Parser
import AgdaShow
import Solver

import CellContext
import qualified Data.Attoparsec.Text as A

import qualified Data.Text.IO as TIO
import System.IO
import qualified Data.Text as T

data Session = Session
  { file      :: String
  , verbose   :: Bool
  , timeout :: Int }

main :: IO ()
main = do
  contents <- TIO.hGetContents stdin
  let parsed = A.parseOnly fileParser contents
  case parsed of
    Right (ctxt, goals) -> solveAndPrint ctxt goals
    Left err -> error $ "Could not parse input: " ++ err

solveAndPrint :: Ctxt -> [Bdy] -> IO ()
solveAndPrint ctxt goals = mapM_ (solveAndPrintGoal ctxt) goals

solveAndPrintGoal :: Ctxt -> Bdy -> IO ()
solveAndPrintGoal ctxt goal = do
  let res = solve ctxt False goal -- Assuming verbose is always False for this function
  TIO.putStrLn $ T.pack $ agdaShow goal res
