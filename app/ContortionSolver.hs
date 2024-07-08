module ContortionSolver where

import Data.Map ((!))
import Data.List

import Control.Monad
import Control.Monad.State

import Contortion
import CellContext

contortionSolver :: Ctxt -> Bdy -> Maybe Term
contortionSolver ctxt phi = msum (map (contort ctxt phi) ctxt)

contort :: Ctxt -> Bdy -> Decl -> Maybe Term
contort ctxt phi@(Bdy m _) (p , Bdy n _) = do
  guard (simpleBdy phi)

  let faces = (sortBy (\(_, s) (_,t) -> compare (varDim ctxt t) (varDim ctxt s))
                [ (ie , bdyFace phi ie) | ie <- restrictions m , sideSpec phi ie])

  sigma <- foldM (\sigma (ie , phiie) -> do
      theta <- case phiie of
          App q rs | q == p -> Just $ injPPMap (cont2pmap rs)
          _ -> do
            let theta = filter (\s -> compareTerms ctxt (App p (pmap2cont s)) phiie)
                        (getPMaps (restrPPMap sigma ie))
            if null theta
              then Nothing
              else Just (combinePMaps theta)
      return $ foldl (\s x -> updatePPMap s (insInd ie x) (theta!x)) sigma (createPoset (m-1)))
    (createPPMap m n)
    faces

  if ppmapNotEmpty sigma
    then return (App p (pmap2cont (fstPMap sigma)))
    else Nothing
