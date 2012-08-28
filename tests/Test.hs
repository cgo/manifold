module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit
import System.Random

import Math.Manifold
import Math.Manifold.Jalla
import Data.List
import Jalla
-- import Jalla.Test

instance Arbitrary CDouble where
  arbitrary = do
    a <- choose (-1000000,1000000) :: Gen Double
    return (realToFrac a)

instance (BlasOps e, Random e) => Arbitrary (Vector e) where
  arbitrary = do
    let m = 100
    -- m   <- choose (1,100)
    els <- vectorOf m (choose (-10000, 10000))
    return $ listVector els


main = defaultMain tests

tests = [
  testGroup "Jalla" [
     testProperty "karcherMean" (prop_karcherMean lvs avg)
     ]
  ]

lvs :: Manifold (Vector CDouble) (Vector CDouble) CDouble
lvs = linearVectorSpace

avg as = (foldr1 (||+) as) |./ (fromIntegral n)
  where n = length as

prop_karcherMean :: (Arbitrary e, Arbitrary t, Arbitrary s, Num t, Num s, Ord s, RealFloat s, Fractional t) => 
                    Manifold e t s -> ([e] -> e) -> [e] -> Bool
prop_karcherMean m f es = 
  case mkm of
       Nothing -> True
       Just km -> d < (realToFrac 1e-10)
         where
           d = mdist m (f es') km
  where 
    mkm = karcherMean m es' 100 (realToFrac 1e-6)
    es' = take 100 es  
