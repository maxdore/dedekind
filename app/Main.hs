{-# LANGUAGE BangPatterns #-}

module Main where

import System.CPUTime
import qualified System.Timeout as T
import Options.Applicative

import Parser
import AgdaShow
import Solver


data Session = Session
  { file      :: String
  , verbose   :: Bool
  , timeout :: Int }

sample :: Parser Session
sample = Session
      <$> strOption
          ( long "file"
         <> short 'f'
         <> help "File containing cell context and problems" )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Whether to be verbose"
         <> showDefault)
      <*> option auto
          ( long "timeout"
         <> short 't'
         <> help "Timeout for each solution attempt"
         <> showDefault
         <> value 10
         <> metavar "INT" )

main :: IO ()
main = do
  Session f _ t <- execParser (info (sample <**> helper)
      ( fullDesc
        <> header "dedekind - a boundary solver for Dedekind cubical type theory" ))
  let v = True
  deb v $ "Solving problems in file " ++ show f
  -- deb v "Parsing..."
  (ctxt , goals) <- loadExample f
  deb v $ "Cell context:\n" ++ show ctxt
  deb v $ show (length goals) ++ " goals."

  mapM_ (\(gi , phi) -> do
    deb v $ "SOLVING " ++ show gi ++ " : " ++ show phi
    start <- getCPUTime
    comp <- T.timeout (t * 1000000) (do
      let !r = solve ctxt phi
      return r)
    case comp of
      Just res -> do
        end <- getCPUTime
        let diff = (end - start) `div` 1000000000
        putStr ("SOLVED IN " ++ show diff ++ "ms")
        print res
        putStrLn $ agdaShow phi res
      Nothing -> putStr "TIMEOUT"
           ) (zip [1..length goals] goals)




deb :: Bool -> String -> IO ()
deb True s = putStrLn s
deb _ _ = return()
