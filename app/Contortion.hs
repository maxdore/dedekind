module Contortion where

import qualified Data.Map as Map
import Data.Map ((!),Map)
import Data.List

type Id = String
type Dim = Int -- the dimension of a cube
type IVar = Int -- De Bruijn indices for variable names

data Endpoint = I0 | I1
  deriving (Eq, Show , Ord)

negI :: Endpoint -> Endpoint
negI I0 = I1
negI I1 = I0

toBool :: Endpoint -> Bool
toBool I0 = False
toBool I1 = True

fromBool :: Bool -> Endpoint
fromBool False = I0
fromBool True = I1

restrictions :: Dim -> [Restr]
restrictions n = [ (i,e) | i <- [1..n], e <- [I0,I1]]

type Restr = (IVar , Endpoint)

predi :: Restr -> Restr
predi (i,e) = (i-1,e)

succi :: Restr -> Restr
succi (i,e) = (i+1,e)

-- When comparing the boundaries of two faces of a cube, we have to adjust
-- de Brujin indices: if i < j, then j is offset by one and vice versa
adji :: Restr -> Restr -> (Restr, Restr)
adji ie je = if fst ie < fst je then (ie,predi je) else (predi ie, je)

swape :: (Endpoint , Endpoint) -> Endpoint -> Endpoint
swape (e,e') | e == e' = id
             | otherwise = negI

swap :: (Restr , Restr) -> Restr -> Restr
swap (ie,je) ke | fst ke == fst ie = (fst je , swape (snd ie, snd je) (snd ke) )
                | fst ke == fst je = (fst ie , swape (snd ie, snd je) (snd ke) )
                | otherwise = ke


-- An element of a poset is a list of 0s and 1s
type Vert = [Endpoint]

type Poset = [Vert]

-- Construct I^n poset
createPoset :: Int -> Poset
createPoset n | n <= 0 = [[]]
createPoset n = let g = (createPoset (n - 1))
  in map (I0 :) g ++ map (I1 :) g

-- Construct x in I^n such that x_i = 1 and 0 otherwise
baseVert :: Int -> Int -> Vert
baseVert n i = (replicate (i-1) I0 ++ [I1] ++ replicate (n-i) I0)


-- Given an element in a poset, remove the i-th index from it
rmInd :: Vert -> Int -> Vert
rmInd ((_:es)) 1 = es
rmInd ((e:es)) n = e : (rmInd es (n-1))
rmInd _ _ = error "This index is not part of the element"

-- Insert e such that x_i = e afterwards
insInd :: Restr -> Vert -> Vert
insInd (0 , _) _ = error "Indices start from 1"
insInd (i , e) es | i > length es + 1 = error "Index too large for element"
                  | otherwise = let (f,s) = splitAt (i-1) es in (f ++ [e] ++ s)

-- Checking order between two elements of a poset
below , above :: Vert -> Vert -> Bool
x `below` y = all (\(e , e') -> toBool e' --> toBool e) (zip x y)
x `above` y = y `below` x

-- The type of poset maps
type PMap = Map Vert Vert

type PPMap = Map Vert [Vert]

createPPMap :: Int -> Int -> PPMap
createPPMap k l = Map.fromList $ map (\v -> (v , createPoset l)) (createPoset k)

-- Give back restriction of sigma, where also domain is restricted
restrPMap :: PMap -> Restr -> PMap
restrPMap sigma (i,e) = Map.mapKeys (`rmInd` i) (Map.filterWithKey (\x _ -> e == x !! (i-1)) sigma)
restrPPMap :: PPMap -> Restr -> PPMap
restrPPMap sigma (i,e) = Map.mapKeys (`rmInd` i) (Map.filterWithKey (\x _ -> e == x !! (i-1)) sigma)

(-->) :: Bool -> Bool -> Bool
x --> y   = not x || y

-- Given a potential substitution, generate all possible substitutions from it
getPMaps :: PPMap -> [PMap]
getPMaps sigma = map Map.fromList (getPMaps' (Map.toList sigma))
  where
  getPMaps' :: [(Vert , [Vert])] -> [[(Vert , Vert)]]
  getPMaps' [] = [[]]
  getPMaps' ((x , vs) : ys) = [ (x , v) : r | v <- vs , r <- getPMaps' (filterRec x v ys) ]

  filterRec :: Vert -> Vert -> [(Vert , [Vert])] -> [(Vert , [Vert])]
  filterRec x v = map (\(y, us) -> (y , [ u | u <- us , (y `below` x) --> (u `below` v) ]))

combinePMaps :: [PMap] -> PPMap
combinePMaps ss = Map.mapWithKey (\x _ -> sort (nub (map (! x) ss))) (head ss)

ppmapNotEmpty :: PPMap -> Bool
ppmapNotEmpty sigma = all (not . null) (map snd (Map.toList sigma))

-- Given a potential substitution, generate the substitution from it
-- (equivalent to head of getPMaps, but perhaps more efficient)
fstPMap :: PPMap -> PMap
fstPMap = Map.fromList . fstPPMap' . Map.toList
  where
  fstPPMap' :: [(Vert , [Vert])] -> [(Vert , Vert)]
  fstPPMap' [] = []
  fstPPMap' ((x,vs) : yws) = (x , head vs) :
    fstPPMap' (map (\(y , ws) -> (y , filter (\w -> (y `below` x) --> (w `below` head vs)) ws)) yws)

injPPMap :: PMap -> PPMap
injPPMap = Map.map (: [])

updatePPMap :: PPMap -> Vert -> [Vert] -> PPMap
updatePPMap sigma x vs = Map.mapWithKey
  (\y us -> filter (\u ->
              (y `above` x) --> any (u `above`) vs &&
              (y `below` x) --> any (u `below`) vs
            ) us) (Map.insert x vs sigma)


type Cont = (IVar , [[[IVar]]])

endpoint2cont :: Endpoint -> [[IVar]]
endpoint2cont I0 = []
endpoint2cont I1 = [[]]

face :: Dim -> Restr -> Cont
face n (i , e) = (n , [ [[j]] | j <- [1..i-1]] ++ [endpoint2cont e] ++ [ [[j]] | j <- [i..n]])

cont2pmap :: Cont -> PMap
cont2pmap (m , rs) = Map.fromList (map (\v -> (v , (map (evalFormula v) rs))) (createPoset m))
  where
  evalFormula :: Vert -> [[IVar]] -> Endpoint
  evalFormula (is) ds =
    let vs1 = map fst $ filter (toBool . snd) (zip [1..] is) in
    let result = map (\d -> filter (\i -> i `notElem` vs1) d) ds in
    fromBool $ [] `elem` result

pmap2cont :: PMap -> Cont
pmap2cont s =
  let domdim = length . fst . head . Map.toList in
  let coddim = length . snd . head . Map.toList in
  (domdim s , map (\fi -> constrFormula (map (\(x , is) -> (x , is !! fi)) (Map.toList s))) [0 .. coddim s-1])
    where
    constrFormula :: [(Vert , Endpoint)] -> [[IVar]]
    constrFormula ves =
      let truevs = [ v | (v , e) <- ves , toBool e ] in
      let cs = [ [ i | (e,i) <- zip vs [1..] , toBool e] | vs <- truevs ] in
      let redcs = filter (\c -> not (any (\d -> c /= d && d `isSubsequenceOf` c) cs)) cs in
      let normcs = sort redcs in
        normcs

unfold :: PPMap -> [Cont]
unfold = (map pmap2cont) . getPMaps

combine :: [Cont] -> PPMap
combine = combinePMaps . (map cont2pmap)
