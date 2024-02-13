module Examples where

import Contortion
import CellContext

-- The examples in this file are for development and testing


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
