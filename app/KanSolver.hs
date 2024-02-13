{-# LANGUAGE FlexibleContexts #-}
module KanSolver where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.List
import Data.Ord
import Data.Maybe

import Debug.Trace

import Contortion
import CellContext

kanSolver :: Ctxt -> Bool -> Bdy -> Term
kanSolver c v phi = head (concatMap (boundKanSolver c v phi) [1..])

boundKanSolver :: Ctxt -> Bool -> Bdy -> Int -> [Term]
boundKanSolver _ _ _ 0 = []
boundKanSolver c v phi@(Bdy d fs) depth = do
  let cdir = (d + 1, I1)

  ope <- opensides phi depth
  sol <- take depth $ evalStateT compCSP (SEnv c phi Map.empty v cdir ope)

  if (null ope)
    then return $ Comp cdir (Bdy (d + 1) sol)
    else do
          rcomps <- foldM
            (\s ie -> do
                let gobdy = if sideSpec phi ie then [(d + 1 , I1) +> Fill cdir phi] else []
                let fphi = normaliseBdy c (bdyFaceBdy c (Bdy (d + 1) (s ++ gobdy)) ie)
                fsol <- kanFiller c fphi ++ boundKanSolver c v fphi (depth - 1)
                return $ s ++ [ie +> fsol]
                )
            sol
            ope
          return $ Comp cdir (Bdy (d + 1) rcomps)

opensides :: Bdy -> Int -> [[Restr]]
opensides phi@(Bdy d fs) depth =
  let unspecs = unspec phi in
  let comps = [ fd | (fd , Comp _ _) <- fs ] in
  let sides = ((restrictions d ++ [(d + 1, I0)]) \\ unspecs ) \\ comps in
    filter (\ops -> length ops <= depth) (sortBy (comparing length) (ps (unspecs ++ sides ++ comps)))
    where
    ps = filterM (const [True, False])


kanFiller :: Ctxt -> Bdy -> [Term]
kanFiller ctxt phi@(Bdy d fs) =
  case unspec phi of
    [] -> [ Fill cd psi | (fd , Comp cd psi) <- fs , fills phi fd (Comp cd psi) ]
    [cd] -> if length [ fd | (fd , Comp cd psi) <- fs ] == 0 then [ Fill cd phi ] else []
    _ -> [] -- here we could in principle fill, but need to make choice for the face in which we are not filling

fills :: Bdy -> Restr -> Term -> Bool
fills (Bdy d fs) fd (Comp cd (Bdy _ gs)) =
  let adfs = [ swap (fd,cd) ie +> t | (ie,t) <- fs , ie /= fd ] in
  sortOn fst adfs  == sortOn fst gs


-- CSP SOLVER
type CVar = Restr

-- The solving monad is a state monad wrapped inside the list search monad
type Solving s a = StateT (SEnv s) [] a

type Domain = [Term]
data CVarInfo a = CVarInfo { delayedConstraints :: Solving a () , values :: Domain}

data SEnv s =
  SEnv { ctxt :: Ctxt
       , goal :: Bdy
       , varMap :: Map CVar (CVarInfo s)
       , verbose :: Bool
       , dir :: Restr -- in which direction do we want a comp
       , open :: [CVar] --  the sides of the cubes that should be left open
       }

-- Management of the constraint solver
lookupDef :: Id -> Solving s Bdy
lookupDef name = do
  c <- gets ctxt
  return $ getDef c name

newVar :: CVar -> Domain -> Solving s CVar
newVar v dom = do
    v `isOneOf` dom
    return v
    where
        x `isOneOf` dom' =
            modify $ \s ->
                let vm = varMap s
                    vi = CVarInfo {
                        delayedConstraints = return (),
                        values = dom'}
                in
                s { varMap = Map.insert x vi vm }

lookupDom :: CVar -> Solving s Domain
lookupDom x = do
    s <- get
    return . values $ varMap s ! x

update :: CVar -> Domain -> Solving s ()
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = i }) vm }
    delayedConstraints vi

addConstraint :: CVar -> Solving s () -> Solving s ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }

type BinaryConstraint s = CVar -> CVar -> Solving s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

getSol :: CVar -> Solving s Term
getSol var = do
  ts <- lookupDom var
  let allsol = concatMap (\s -> do
           case s of
             PApp t ss -> map (App t) (unfold ss)
             t -> [t]
           ) ts
  phi@(Bdy d fs) <- gets goal
  (gi,ge) <- gets dir
  c <- gets ctxt
  let msol = if fst var == gi || not (sideSpec phi var)
        then allsol
        else filter (\t -> termFace c t (gi-1,ge) == bdyFace phi var) allsol
  guard (not (null msol))
  sol <- lift msol
  update var [sol]
  return sol

debug :: [String] -> Solving s ()
debug outs = do
  v <- gets verbose
  when v (mapM_ traceM outs)

compCSP :: Solving s [Face]
compCSP = do
  phi@(Bdy d fs) <- gets goal
  c <- gets ctxt
  (gi,ge) <- gets dir
  ope <- gets open

  debug ["SOLVE IN " ++ show (gi,ge) ++ " FOR " ++ show phi ++ " WITH OPEN SIDES " ++ show ope]

  -- The sides of the cube that need to be filled
  let solv = (restrictions d ++ [(gi,negI ge)]) \\ ope

  -- The set of terms that can be used
  let pterms = [ PApp p (createPPMap d n) | (p , Bdy n _) <- c ]

  debug [show pterms]

  sides <- mapM (\f@(i,_) ->
                      if i == gi || not (sideSpec phi f)
                        -- if we have the back of the cube or the side of the
                        -- goal is not specified, we have a domain containing
                        -- all pterms
                        then newVar f pterms
                        -- otherwise we restrict the initial domains to match
                        -- the goal boundary
                        else do
                          let gf = bdyFace phi f
                          case gf of
                            Comp cd t -> newVar f [Fill cd t]
                            _ -> do
                              v <- newVar f (catMaybes $ map (\t -> restrPTerm c t (gi-1,ge) [gf]) pterms)
                              return v
            ) solv

  debug ["AFTER INIT"] >> mapM (\s -> lookupDom s >>= \r -> return (show (s , r))) sides >>= debug
  mapM_ (uncurry boundaryConstraint) [ (f,g) | (f:ys) <- tails solv, g <- ys , fst f /= fst g ]
  debug ["AFTER SIDES"] >> mapM (\s -> lookupDom s >>= \r -> return (show (s , r))) sides >>= debug
  mapM (\f -> getSol f >>= \s -> return (f,s)) sides

-- Constraint management

restrPTerm :: Ctxt -> Term -> Restr -> [Term] -> Maybe (Term)
restrPTerm c (PApp t ss) ie hs =
  let ss' = filter (\s -> termFace c (App t s) ie `elem` hs) (unfold ss) in
    if null ss'
      then Nothing
      else Just (PApp t (combine ss'))
restrPTerm c t ie hs = if termFace c t ie `elem` hs then Just t else Nothing

ptermFace :: Ctxt -> Term -> Restr -> [Term]
ptermFace c (PApp t ss) ie = nub $ map (\s -> termFace c (App t s) ie) (unfold ss)
ptermFace c t ie = [termFace c t ie]


matchGoal :: CVar -> Solving s ()
matchGoal cv = addConstraint cv $ do
  c <- gets ctxt
  (gi,ge) <- gets dir
  phi@(Bdy d fs) <- gets goal
  ts <- lookupDom cv

  let ts' = catMaybes $ map (\t -> restrPTerm c t (gi-1,ge) [bdyFace phi cv]) ts
  guard (not (null ts'))
  when (ts' /= ts) $ update cv ts'

boundaryConstraint :: Restr -> Restr -> Solving s ()
boundaryConstraint = addBinaryConstraint $ \cv dv -> do
  c <- gets ctxt
  let (cf , df) = adji cv dv

  ts <- lookupDom cv
  ss <- lookupDom dv
  let tsf = concatMap (\t -> ptermFace c t df) ts
  let ssg = concatMap (\t -> ptermFace c t cf) ss

  let hs = tsf `intersect` ssg

  guard (not (null hs))

  let ts' = catMaybes $ map (\t -> restrPTerm c t df hs) ts
  let ss' = catMaybes $ map (\t -> restrPTerm c t cf hs) ss

  when (ts' /= ts) $ update cv ts'
  when (ss' /= ss) $ update dv ss'

