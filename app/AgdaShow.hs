module AgdaShow where

import Data.List

import Contortion
import CellContext

import Debug.Trace

dimNames :: [String]
dimNames = ["i","j","k","l","m","n","o","p","q"]

agdaEndpoint :: Endpoint -> String
agdaEndpoint I0 = "i0"
agdaEndpoint I1 = "i1"

dimName :: IVar -> [IVar] -> String
dimName i skip = [ d | (j,d) <- zip [1..] dimNames , not (j `elem` skip) ]!!(i-1)

sparseParen int [s] = s
sparseParen int ss = "(" ++ concat (intersperse int ss) ++ ")"

agdaClause :: [IVar] -> [IVar] -> String
agdaClause (ls) skip = sparseParen " ∧ " (map (\(i) -> dimName i skip) ls)

agdaFormula :: [[IVar]] -> [IVar] -> String
agdaFormula [] _ = "i0"
agdaFormula [[]] _ = "i1"
agdaFormula cs skip = sparseParen " ∨ " (map (\c -> agdaClause c skip) cs)

agdaAbstract :: [Int] -> String
agdaAbstract is = "\955 " ++ concat (intersperse " " (map (\i -> dimName i []) is)) ++ " \8594 "

agdaInd :: Int -> String
agdaInd d = concat (replicate d "  ")



bind :: Int -> [Int] -> [Int]
bind i js = sort $ free!!(i-1) : js
  where
    free = [1..] \\ js

agdaBox :: Int -> [Int] -> Restr -> [Face] -> Bool -> String
agdaBox d skip fd fs isfill = "(" ++ agdaAbstract [d+1] ++ "\955 {\n" ++
  concatMap (\((i,e) , t) ->
               agdaInd d ++ (if (i,e) == fst (head fs) then "  " else "; ") ++ "(" ++
                 show skip ++ " :: " ++ (dimName i skip) ++ " = " ++ agdaEndpoint e ++ ") \8594 " ++ agdaTerm (d+1) (bind i skip) t ++ "\n")
     [ f | f <- fs , fst (fst f) /= fst fd ]
  ++ agdaInd d ++ "})" ++
  if isfill
    then " (inS (" ++ agdaTerm d skip (head [ t | ((i,e),t) <- fs , i == fst fd , negI e == snd fd ]) ++ ")) " ++ dimName (fst fd) skip
    else " (" ++ agdaTerm d skip (head [ t | ((i,e),t) <- fs , i == fst fd , negI e == snd fd ]) ++ ")"


agdaTerm :: IVar -> [IVar] -> Term -> String
agdaTerm d skip (App p (_ , psi)) = -- show skip ++ " :: " ++
  p ++ " " ++ (concat (intersperse " " (map (\f -> agdaFormula f skip) psi)))
agdaTerm d skip (Comp fd (Bdy d' fs)) = "hcomp (" ++ agdaAbstract [d+1] ++ "\955 {\n" ++
  concatMap (\((i,e) , t) ->
               agdaInd d ++ (if (i,e) == fst (head fs) then "  " else "; ") ++ "(" ++
                 -- show skip ++ " :: " ++
                 (dimName i skip) ++ " = " ++ agdaEndpoint e ++ ") \8594 " ++ agdaTerm (d+1) (bind i skip) t ++ "\n")
     [ f | f <- fs , fst (fst f) /= fst fd ]
  ++ agdaInd d ++ "}) (" ++ agdaTerm d skip (head [ t | ((i,e),t) <- fs , i == fst fd , negI e == snd fd ]) ++ ")"

agdaTerm d skip (Fill fd (Bdy d' fs)) = "hfill (" ++ agdaAbstract [d+1] ++ "\955 {\n" ++
  concatMap (\((i,e) , t) ->
               agdaInd d ++ (if (i,e) == fst (head fs) then "  " else "; ") ++ "(" ++
                 -- show skip ++ " :: " ++
                 (dimName i (bind (fst fd) skip)) ++ " = " ++ agdaEndpoint e ++ ") \8594 " ++ agdaTerm (d+1) (bind i (bind (fst fd) skip)) t ++ "\n")
     [ f | f <- fs , fst (fst f) /= fst fd ]
  ++ agdaInd d ++ "}) (inS (" ++ agdaTerm d skip (head [ t | ((i,e),t) <- fs , i == fst fd , negI e == snd fd ]) ++ ")) " ++ dimName (fst fd) skip



agdaShow :: Bdy -> Term -> String
agdaShow (Bdy d _) t = agdaAbstract [1..d] ++ agdaTerm 2 [] t
