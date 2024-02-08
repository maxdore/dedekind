{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- import Data.Word
import Data.List
import Data.Maybe
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad
-- import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- import Debug.Trace

import Contortion
import CellContext

type IVarName = String

parseId = many1 (letter <|> char '\'')

parseEndpoint :: Parser Endpoint
parseEndpoint = do
  ev <- char '0' <|> char '1'
  return $ if ev == '0' then I0 else I1

parseDecl :: Parser Decl
parseDecl = do
  var <- parseId
  skipSpace
  char ':'
  skipSpace
  boundary <- parseBdy []
  endOfLine
  return (var, boundary)

parseBdy :: [IVarName] -> Parser Bdy
parseBdy ivs = do
  char '('
  ivs' <- parseId `sepBy` char ' '
  char ')'
  -- traceShowM ivs'
  char '['
  fgs <- parseAssign (ivs++ivs') `sepBy` char '|'
  char ']'
  return $ Bdy (length ivs') fgs

debruijn :: IVarName -> [IVarName] -> Parser IVar
debruijn iv ivs = do
  when (iv `notElem` ivs) (error "Interval variable not bound in boundary")
  return $ fromJust (elemIndex iv ivs) + 1


parseAssign :: [IVarName] -> Parser (Restr, Term)
parseAssign ivs = do
  -- traceM "PARSE ASSIGN"
  skipSpace
  iv <- parseId
  i <- debruijn iv ivs
  skipSpace
  char '='
  skipSpace
  e <- parseEndpoint
  -- traceShowM (i,e)
  skipSpace
  string "->"
  skipSpace
  t <- parseTerm (delete iv ivs)
  skipSpace
  -- traceShowM t
  return ((i,e) , t)

parseTerm :: [IVarName] -> Parser Term
parseTerm ivs = parseComp ivs <|> parseApp ivs <|> parsePoint ivs

parseComp :: [IVarName] -> Parser Term
parseComp ivs = do
  string "hcomp"
  skipSpace
  (Bdy _ tube) <- parseBdy ivs
  skipSpace
  back <- parseTerm ivs
  return $ Comp (length ivs+1 , I1) (Bdy (length ivs+1) (tube ++ [(length ivs+1 , I0) +> back]))

parseApp :: [IVarName] -> Parser Term
parseApp ivs = do
  p <- parseId
  char '('
  rs <- (parseFaceFormula <|> parseFormula ivs) `sepBy` char ','
  char ')'
  return (App p (length ivs , rs))

parsePoint :: [IVarName] -> Parser Term
parsePoint ivs = do
  p <- parseId
  return (App p (length ivs,[]))


parseFaceFormula :: Parser [[IVar]]
parseFaceFormula = do
  e <- parseEndpoint
  return $ if e == I0 then [] else [[]]

parseFormula :: [IVarName] -> Parser [[IVar]]
parseFormula ivs = do
  -- traceShowM ivs
  raw <- parseClauses <|> (parseId >>= \iv -> return [[iv]])
  -- traceShowM raw
  mapM (mapM (`debruijn` ivs)) raw

parseClauses :: Parser [[IVarName]]
parseClauses = parseDisj `sepBy1` string " \\/ "

parseDisj :: Parser [IVarName]
parseDisj = parseLiterals <|> (parseId >>= \iv -> return [iv]) 

parseLiterals :: Parser [IVarName]
parseLiterals = parseId `sepBy1` string " /\\ "


comment :: Parser ()
comment = do
  string "--"
  _ <- (manyTill anyChar endOfLine)
  skipSpace
  return ()


parseGoal :: Parser Bdy
parseGoal = do
  many1 endOfLine
  skipMany comment
  skipSpace
  string "? :"
  skipSpace
  goal <- parseBdy []
  return goal

fileParser :: Parser (Ctxt , [Bdy])
fileParser = do
  decls <- manyTill parseDecl (string "---")
  goals <- many parseGoal
  return (decls , map (normaliseBdy decls) goals)

loadExample :: String -> IO (Ctxt , [Bdy])
loadExample filename = do
  file <- TIO.readFile filename
  print file
  case parseOnly fileParser file of
    Right res -> return res
    Left err -> error $ "Could not parse file" ++ err
