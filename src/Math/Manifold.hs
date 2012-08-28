module Math.Manifold
       ( -- * The Manifold Data Type
         Manifold(..),
         -- * Computations on Manifolds
         karcherMean) where


{-| Data type describing operations on elements of a manifold. 
    /e/ is the element type, /t/ the tangent type, and /s/ is a scalar type. -}
data Manifold e t s = Manifold {
  -- | Division of a tangent space element by a /s/.
  mdiv :: t -> s -> t,
  -- | Multiplication of a tangent space element by a /s/.
  mmul :: t -> s -> t,
  -- | Exponential map. /mexp a t/ is the exponential map of tangent /t/ at /e/.
  mexp :: e -> t -> e,
  -- | Logarithmic map. /mlog a b/ is the logarithmic map of /b/ at /a/.
  mlog :: e -> e -> t,
  -- | Inner product. /minner a t1 t2/ is the inner product of /t1,t2/ at /a/.
  minner :: e -> t -> t -> s,
  -- | Distance. /mdist a b/ is a distance from /a/ to /b/.
  mdist :: e -> e -> s }


{-| Compute the Karcher mean of a list of elements, given a manifold. -}
karcherMean :: (Num t, Num s, Ord s, Fractional t) => 
               Manifold e t s   -- ^ Manifold to compute on.
               -> [e]            -- ^ Elements to compute the mean of.
               -> Int            -- ^ Max. number of iterations to do.
               -> s              -- ^ Epsilon: if result does not change more than /epsilon/, the computation is stopped.
               -> Maybe e        -- ^ The result, or /Nothing/ in case the input list was empty.
karcherMean _ [] _ _ = Nothing
karcherMean m es@(e:_) maxIter epsilon = mean
  where
    mean = Just $ karcherMean' m e es n maxIter epsilon
    n = length es


-- sum: braucht Num. tangentSum in Manifold hinzufuegen.
-- Es kommen nicht alle Typen in der Funktion vor, daher ist sie nicht eindeutig ...
-- Sollten die FunDeps oben nicht dafuer sorgen, dass m die anderen Typen
-- /eindeutig/ bestimmt??
-- /Note/: This is not very efficient for /Vector/, because the Fractional instance is
-- used for division. That means a constant is copied into a vector and then that is used for division.
-- Not very good. Therefore, specialise this for /Vector/ (to do).
-- NOTE: sum does not work here, because it starts with a "0"! Therefore we use a foldr1, see below.
karcherMean' :: (Num s, Num t, Ord s, Fractional t) => Manifold e t s -> e -> [e] -> Int -> Int -> s -> e
karcherMean' m currentMean es itemCount 0 epsilon = currentMean
karcherMean' m currentMean es itemCount maxIter epsilon =
  if (minner m currentMean next next) < epsilon 
  then nextEl 
  else karcherMean' m nextEl es itemCount maxIter' epsilon
  where
    next = mdiv m s (fromIntegral itemCount)
    s = (foldr1 (+) $ map (mlog m currentMean) es)
    nextEl = mexp m currentMean next
    maxIter' = maxIter - 1    
