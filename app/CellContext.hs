module CellContext where

-- import qualified Data.Map as Map
-- import Data.Map ((!),Map)
import Data.List
import Data.Maybe

import Contortion

-- import Debug.Trace



  -- Fill dir ty means fill type ty in direction dir
 -- TODO explain Comp
 -- We have back side of the open cube in the bdy with inverted fill direction


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
    -- traceShow ("SUBST" , cs , i , ds) $
  let res = [ delete i c ++ d | c <- cs , i `elem` c , d <- ds ] ++ [ c | c <- cs , i `notElem` c ] in
    -- traceShow (cs , i , ds , res) $
    map sort res

redDnf :: [[IVar]] -> [[IVar]]
redDnf cs = [ c | c <- cs , all (\d -> not (d /= c && d `isSubsequenceOf` c)) cs ]

compose (m , ss) (n , rs) =
  -- trace ("COMPOSING" ++ show (m , ss) ++ " with " ++ show (n , rs)) $
  if m /= length rs
    then error $ "Can't compose " ++ show (m , ss) ++ " with " ++ show (n , rs)
    else
      let rs' = map (map (map (\i -> i + m))) rs in
        -- traceShow rs' $
      let ss' = map (\d -> foldr (\i d' -> subst d' i (rs'!!(i-1))) d [1..m]) ss in
        -- traceShow ss' $
      let ss'' = map (map (map (\i -> i - m))) ss' in
      (n , map redDnf ss'')


normaliseBdy :: Ctxt -> Bdy -> Bdy
normaliseBdy ctxt (Bdy d fs) = Bdy d (map (\(ie,t) -> ie +> normalise ctxt t) fs)


normalise :: Ctxt -> Term -> Term
normalise ctxt (App p (n , rs)) = 
  -- trace ("NORMALISING" ++ show (App p (n , rs))) $
  let isface = case findIndex null rs of
                  Just i -> Just (i+1,I0)
                  Nothing -> case findIndex ([] `elem`) rs of
                    Just i -> Just (i+1,I1)
                    Nothing -> Nothing in
  -- traceShow isface $
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


-- instance Eq Term where
--   App p rs == App q ss = normalise p == normalise q

compareTerms :: Ctxt -> Term -> Term -> Bool
compareTerms ctxt t s = normalise ctxt t == normalise ctxt s


-- Syntactic sugar to allow writing (1,I0) +> t
(+>) :: Restr -> Term -> Face
r +> t = (r , t)

type Face = (Restr , Term )

data Bdy = Bdy Dim [Face]

-- To check equality of types we need to order their faces
-- (Not well-defined since we need to be able to reduce terms in a cell context...)
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


-- necessary to only call contort if contortable
simpleBdy :: Bdy -> Bool
simpleBdy (Bdy d fs) = all (\(ie,t) -> isApp t) fs


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
  -- trace ("COMPUTE BOUNDARY OF " ++ show (Bdy d fs) ++ " AT " ++ show ie) $
  Bdy (d-1) [ je' +> termFace c t ie' | (je,t) <- fs , fst je /= fst ie ,
                                       let (je',ie') = adji je ie,
                                       sideSpec (inferBdy c t) ie' ]

varDim :: Ctxt -> Term -> Dim
varDim c (App p _) = let Bdy d _ = (getDef c p) in d


-- simple examples
p = [("p" , Bdy 1 [])]
por = App "p" (2 , [[[1],[2]]])
pandorand = App "p" (3 , [[[1,2],[2,3]]])

s = [("a",Bdy 0 []),("s" , Bdy 2 [(1,I0) +> App "a" (1,[]),(1,I1) +> App "a" (1,[]),(2,I0) +> App "a" (1,[]),(2,I1) +> App "a" (1,[])])]
sandor = App "s" (2 , [[[1,2]],[[1],[2]]])
ehhelp = App "s" (3 , [[[1,2],[2,3]],[[1]]])


pqr = [("p" , Bdy 1 []) , ("q" , Bdy 1 [(1,I0) +> App "p" (0 , [[[]]])]) , ("r" , Bdy 1 [(1,I0) +> App "q" (0 , [[[]]])])]

pqrthreep = Bdy 1 [(1,I0) +> App "p" (0 , [[]]) , (1,I1) +> App "r" (0 , [[[]]])]

pqrassoc = (Bdy 2 [
    (1,I0) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "p" (1,[[]])
    , (1,I1) +>    App "r" (1,[[[1]]])
    , (2,I0) +>    Comp (2,I1) (Bdy 2 [
                      (1,I0) +>    App "p" (1,[[]])
                    , (1,I1) +>    App "q" (1,[[[1]]])
                    , (2,I0) +>    App "p" (1,[[[1]]])
                    ])
    ])
   , (1,I1) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "p" (1,[[]])
    , (1,I1) +>    Comp (2,I1) (Bdy 2 [
                      (1,I0) +>    App "p" (1,[[[]]])
                    , (1,I1) +>    App "r" (1,[[[1]]])
                    , (2,I0) +>    App "q" (1,[[[1]]])
                    ])
    , (2,I0) +>    App "p" (1,[[[1]]])
    ])
  , (2,I0) +>    App "p" (1,[[]])
  , (2,I1) +>    App "r" (1,[[[]]])
  ])

pqrassocback = (Bdy 2 [
    (1,I0) +> Comp (2,I1) (Bdy 2 [
      (1,I0) +> App "p" (1,[[]])
    , (1,I1) +> App "q" (1,[[[1]]])
    , (2,I0) +> App "p" (1,[[[1]]])
    ])
   , (1,I1) +> App "p" (1,[[[1]]])
   , (2,I0) +> App "p" (1,[[]])
  ])

pqrassocright = (Bdy 2 [
     (1,I0) +> App "r" (1,[[[1]]])
   , (1,I1) +> Comp (2,I1) (Bdy 2 [
      (1,I0) +> App "p" (1,[[[]]])
    , (1,I1) +> App "r" (1,[[[1]]])
    , (2,I0) +> App "q" (1,[[[1]]])
    ])
   , (2,I0) +> Comp (2,I1) (Bdy 2 [
                    (1,I0) +>    App "q" (1,[[[1]]])
                  , (1,I1) +>    App "p" (1,[[[]]])
                  , (2,I0) +>    App "p" (1,[[[]]])])
   , (2,I1) +> App "r" (1,[[[]]])
  ])

  

-- _ : (i j)[i = 0 -> hcomp (k)[j = 0 -> p(0) | j = 1 -> r(k) ]
--                             hcomp (k)[j = 0 -> p(0) | j = 1 -> q(k) ] p(j) |
--           i = 1 -> hcomp (k)[j = 0 -> p(0) |
--                              j = 1 -> hcomp (l)[k = 0 -> q(0) | k = 1 -> r(l) ] q(k) ]
--                              p(j) |
--           j = 0 -> p(0) |
--           j = 1 -> r(1) ]

  

sqexp = 
  [("x",(Bdy 0 [])),("p",(Bdy 1 [
    (1,I0) +>    App "x" (0,[])
  , (1,I1) +>    App "x" (0,[])
  ])),("q",(Bdy 1 [
    (1,I0) +>    App "x" (0,[])
  , (1,I1) +>    App "x" (0,[])
  ])),("alpha",(Bdy 2 [
    (1,I0) +>    App "p" (1,[[[1]]])
  , (1,I1) +>    App "p" (1,[[[1]]])
  , (2,I0) +>    App "q" (1,[[[1]]])
  , (2,I1) +>    App "q" (1,[[[1]]])
  ]))]



-- TODO BELOW SHOULD HAVE BDY 2!!!
sqtocomp = (Bdy 2 [
    (1,I0) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "x" (1,[])
    , (1,I1) +>    App "q" (1,[[[1]]])
    , (2,I0) +>    App "p" (1,[[[1]]])
    ])
  , (1,I1) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "x" (1,[])
    , (1,I1) +>    App "p" (1,[[[1]]])
    , (2,I0) +>    App "q" (1,[[[1]]])
    ])
  , (2,I0) +>    App "x" (1,[])
  , (2,I1) +>    App "x" (1,[])
  ])

sqtocompright = (Bdy 2 [
    (1,I0) +> App "q" (1,[[[1]]])
  , (1,I1) +> App "p" (1,[[[1]]])
  , (2,I1) +> App "x" (1,[])
  ])

sqtocompback = (Bdy 2 [
    (1,I0) +> App "p" (1,[[[1]]])
  , (1,I1) +> App "q" (1,[[[1]]])
  , (2,I0) +> App "x" (1,[])
  -- , (2,I1) +> Comp (2,I1) (Bdy 2 [
  --     (1,I0) +>    App "p" (1,[[[1]]])
  --   , (1,I1) +>    App "x" (1,[])
  --   , (2,I0) +>    App "q" (1,[[[1]]])
  --   ])
   ])

lcancel = [("p",(Bdy 1 [])),
       ("q",(Bdy 1 [
          (1,I0) +>    App "p" (0,[[]])
        ]))]

lcancelsimple = (Bdy 2 [
    (1,I0) +>    App "q" (1,[[[1]]])
  , (1,I1) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "p" (1,[[[]]])
    , (1,I1) +>    App "q" (1,[[[1]]])
    , (2,I0) +>    Comp (2,I1) (Bdy 2 [
          (1,I0) +>    App "p" (1,[[[1]]])
        , (1,I1) +>    App "p" (1,[[]])
        , (2,I0) +>    App "p" (1,[[]])
        ])
    ])
  , (2,I0) +>    App "p" (1,[[[1]]])
  , (2,I1) +>    App "q" (1,[[[]]])
  ])


lcancel2comp = (Bdy 2 [
    (1,I0) +>    App "q" (1,[[[1]]])
  , (1,I1) +>    Comp (2,I1) (Bdy 2 [
      (1,I0) +>    App "p" (1,[[]])
    , (1,I1) +>    Comp (2,I1) (Bdy 2 [
          (1,I0) +>    App "p" (1,[[[]]])
        , (1,I1) +>    App "q" (1,[[[1]]])
        , (2,I0) +>    Comp (2,I1) (Bdy 2 [
            (1,I0) +>    App "p" (1,[[[1]]])
          , (1,I1) +>    App "p" (1,[[]])
          , (2,I0) +>    App "p" (1,[[]])
          ])
        ])
    , (2,I0) +>    App "p" (1,[[[1]]])
    ])
  , (2,I0) +>    App "p" (1,[[]])
  , (2,I1) +>    App "q" (1,[[[]]])
  ])
