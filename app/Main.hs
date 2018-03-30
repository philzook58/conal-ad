{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}

module Main where

import Lib
import Control.Category.Constrained.Prelude
--import Prelude hiding (id, (.), uncurry, curry)


newtype D k a b = D (a -> (b, k a b))

linearD :: (a -> b) -> (k a b) -> D k a b
linearD f f' = D (\a -> (f a, f'))

instance Category k => Category (D k) where
    type (Object (D k) o) = (Additive o, (Object k o)) -- constraints that the objects must satisfy
    id = linearD id id
    (D g) . (D f) = D (\a -> let (b , f') = f a
                                 (c , g') = g b in (c, g' . f')) 


-- could use Kmett's Additive from the linear package, but  is rank 1
-- also Data.AdditiveGroup from conal's vector-space


instance Cartesian k => Cartesian (D k) where
   type (PairObject (D k) a b) = PairObject k a b
   type (UnitObject (D k) a) = UnitObject k a 
   swap = linearD swap swap
   attachUnit = linearD attachUnit attachUnit
   detachUnit = linearD detachUnit detachUnit
   regroup = linearD regroup regroup
   regroup' = linearD regroup' regroup'




-- morphism is basically what Conal calls being Monoidal
-- and *** = Big Cross
instance Morphism k => Morphism (D k) where
    (D f) (***) (D g) = D $ \(a, b) -> let (c,f') = f a
                                           (d, g') = g b in
                                           ((c,d, f' *** g'))
                                             -- paralell arrows

-- exl = fst
-- exr = snd
-- triangle = &&&

dup = id &&& id

instance PreArrow a => PreArrow (D a) where
    fst = linearD fst fst
    snd = linearD snd snd
    (&&&) = (***) . dup' where dup' = linearD dup dup
    terminal = linearD terminal terminal



class Additive o where
    addy :: o -> o -> o
    zero :: o
{-
instance (Num a) => Additive a where
    addy = (+)
    zero = fromInteger 0
-}

instance Additive Double where
    addy = (+)
    zero = fromInteger 0

class NumCat k a where
    negateC :: k a a
    addC :: k (a, a) a
    mulC :: k (a, a) a

instance Num a => NumCat (->) a where
    negateC = negate
    addC (x, y) = x + y
    mulC (x,y) = x * y

class Scalable k a where
    scale :: a -> k a a

--instance Num a => Scalable    
{-
instance Scalable k s => NumCat (D k) s where
    negateC = linearD negateC
    addC = linearD addC
    mulC = D (\(a,b) -> (a * b,  )
-}



main :: IO ()
main = someFunc
