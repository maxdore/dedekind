module CellContext where

import Data.List
import Data.Maybe

import Contortion

data Term = App Id Cont
          | PApp Id PPMap
          | Fill Restr Bdy
          | Comp Restr Bdy
  deriving (Eq, Show)

isApp :: Term -> Bool
isApp (App _ _) = True
isApp _ = False


subst :: [[IVar]] -> IVar -> [[IVar]] -> [[IVar]]
subst cs i ds =
  let res = [ delete i c ++ d | c <- cs , i `elem` c , d <- ds ] ++ [ c | c <- cs , i `notElem` c ] in
    map sort res

redDnf :: [[IVar]] -> [[IVar]]
redDnf cs = [ c | c <- cs , all (\d -> not (d /= c && d `isSubsequenceOf` c)) cs ]

compose :: Cont -> Cont -> Cont
compose (m , ss) (n , rs) =
  if m /= length rs
    then error $ "Can't compose " ++ show (m , ss) ++ " with " ++ show (n , rs)
    else
      let rs' = map (map (map (\i -> i + m))) rs in
      let ss' = map (\d -> foldr (\i d' -> subst d' i (rs'!!(i-1))) d [1..m]) ss in
      let ss'' = map (map (map (\i -> i - m))) ss' in
      (n , map redDnf ss'')


normaliseBdy :: Ctxt -> Bdy -> Bdy
normaliseBdy ctxt (Bdy d fs) = Bdy d (map (\(ie,t) -> ie +> normalise ctxt t) fs)


normalise :: Ctxt -> Term -> Term
normalise ctxt (App p (n , rs)) = 
  let isface = case findIndex null rs of
                  Just i -> Just (i+1,I0)
                  Nothing -> case findIndex ([] `elem`) rs of
                    Just i -> Just (i+1,I1)
                    Nothing -> Nothing in
  case (isface ) of
    Just (i,e) ->
      let bdy = (getDef ctxt p) in
      if sideSpec bdy (i,e) then
        let App q (m , ss) = bdyFace bdy (i,e) in
        normalise ctxt (App q (compose (m , ss) (n , take (i-1) rs ++ drop i rs)))
      else App p (n , rs)
    _ -> App p (n , rs)

normalise ctxt (Comp ie phi) = Comp ie (normaliseBdy ctxt phi)
normalise ctxt (Fill ie phi) = Fill ie (normaliseBdy ctxt phi)

compareTerms :: Ctxt -> Term -> Term -> Bool
compareTerms ctxt t s = normalise ctxt t == normalise ctxt s

-- Syntactic sugar to allow writing (1,I0) +> t
(+>) :: Restr -> Term -> Face
r +> t = (r , t)

type Face = (Restr , Term )

data Bdy = Bdy Dim [Face]

-- To check equality of types we need to order their faces
instance Eq Bdy where
  Bdy d fs ==  Bdy d' fs' = d == d' && sortOn fst fs == sortOn fst fs'

-- Print faces of a cube on different lines and indented
instance Show Bdy where
  show (Bdy d []) = "(Bdy " ++ show d ++ " [])"
  show (Bdy d fs) = "(Bdy " ++ show d ++ " [\n  " ++
    intercalate ", " (map
                      (\(f,t) -> show f ++ " +> " ++ concatMap (\l ->  "   " ++ l ++ "\n") (lines (show t)))
                      (sortOn fst fs))
    ++ "])"

type Decl = (Id , Bdy)
type Ctxt = [Decl]


-- check if boundary only contains contorted cells
simpleBdy :: Bdy -> Bool
simpleBdy (Bdy d fs) = all (\(_,t) -> isApp t) fs

getDef :: Ctxt -> Id -> Bdy
getDef c name =
  case lookup name c of
    Just face -> face
    Nothing -> error $ "Could not find definition of " ++ name


inferBdy :: Ctxt -> Term -> Bdy
inferBdy _ (Fill ie (Bdy d fs)) = Bdy d ((ie +> Comp ie (Bdy d fs)) : fs)
inferBdy c (Comp ie ty) = bdyFaceBdy c ty ie
inferBdy c (App t psi@(m , _)) = Bdy m [ (i,e) +> normalise c (App t (compose psi (face (m-1) (i,e)))) | i <- [1..m] , e <- [I0,I1] ]
inferBdy c (PApp t rs) = undefined


bdyFace :: Bdy -> Restr -> Term
bdyFace ty@(Bdy _ fs) f =
  case lookup f fs of
    Just face -> face
    Nothing -> error $ "Could not find face " ++ show f ++ " of " ++ show ty

termFace :: Ctxt -> Term -> Restr -> Term
termFace ctxt (App t psi@(m , _)) ie = normalise ctxt (App t (compose psi (face (m-1) ie)))
termFace c t ie = bdyFace (inferBdy c t) ie

sideSpec :: Bdy -> Restr -> Bool
sideSpec (Bdy _ fs) f = isJust (lookup f fs)

unspec :: Bdy -> [Restr]
unspec (Bdy d fs) = restrictions d \\ map fst fs

-- Computes boundary of an unspecified side of a boundary
bdyFaceBdy :: Ctxt -> Bdy -> Restr -> Bdy
bdyFaceBdy c (Bdy d fs) ie =
  Bdy (d-1) [ je' +> termFace c t ie' | (je,t) <- fs , fst je /= fst ie ,
                                       let (je',ie') = adji je ie,
                                       sideSpec (inferBdy c t) ie' ]

varDim :: Ctxt -> Term -> Dim
varDim c (App p _) = let Bdy d _ = (getDef c p) in d
