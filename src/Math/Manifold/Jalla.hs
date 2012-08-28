{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
module Math.Manifold.Jalla
(linearVectorSpace) where

import Numeric.Jalla
--import Math.Vector
--import BLAS.Foreign.BlasOps
import Math.Manifold

linearVectorSpace :: (CVector vec e, VectorVector vec e) => Manifold (vec e) (vec e) e
linearVectorSpace = Manifold { 
  mdiv = \t s -> vectorMap (/s) t
  , mmul = \t s -> vectorMap (*s) t
  , mexp = \e t -> modifyVector e $ vectorAdd 1 t
  , mlog = mlog'
  , minner = minner'
  , mdist = \e1 e2 -> let v = mlog' e1 e2 in sqrt (minner' e1 v v) }

mlog' e1 e2 = modifyVector e2 $ vectorAdd (-1) e1
minner' _ t1 t2 = t1 ||* t2