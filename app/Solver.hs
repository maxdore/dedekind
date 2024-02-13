module Solver where

import CellContext
import ContortionSolver
import KanSolver


-- TODO ALSO CALL KANFILLER

solve :: Ctxt -> Bool -> Bdy -> Term
solve ctxt v phi =
  case contortionSolver ctxt phi of
    Just t -> t
    Nothing -> case kanFiller ctxt phi of
      (t:_) -> t
      [] -> kanSolver ctxt v phi
