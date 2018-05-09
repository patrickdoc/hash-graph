{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AbstractGraph.Impl.Hash where

import Data.AbstractGraph.Class

-- Classes
import Control.Comonad
import Data.Profunctor

-- HashG
import Data.Hashable
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM

----------------------------------------
-- HashMap based implementation of total graphs

-- FIX PARTIAL ASSUMPTIONS: i.e. foldable instance is wrong

data HashG n e = HG
  { unHG :: HashMap n (HashMap n e)
  , defaultVal :: e }

lift0 :: e -> HashG n e
lift0 = HG empty

instance (Eq n, Hashable n) => Graph (HashG n e) where
  type Node (HashG n e) = n
  type Edge (HashG n e) = e

  constant = lift0

  -- This is bad
  sample (HG g d) = \n1 n2 -> lookupDefault d n2 (lookupDefault empty n1 g)

  updateWith f (HG g d) src dst e = HG (adjust (adjust (f e) dst) src g) d

instance (Eq n, Hashable n, Semigroup e) => Semigroup (HashG n e) where
  (HG f df) <> (HG g dg) = HG (unionWith (unionWith (<>)) f g) (df <> dg)

instance (Eq n, Hashable n, Monoid e) => Monoid (HashG n e) where
  mempty = lift0 mempty

instance (Eq n, Hashable n) => FinGraph (HashG n e) where
  nodes (HG g _) = keys g

instance Foldable (HashG n) where
  foldr f acc (HG g _) = HM.foldr (flip (HM.foldr f)) acc g

instance Functor (HashG n) where
  fmap f (HG g d) = HG (fmap (fmap f) g) (f d)

-- Have to deal with keys missing from either side
-- instance Applicative (HashG n) where
--   pure = constant
--   (HG f fd) <*> (HG g gd) = HG (unionWith (unionWith ($)) f g) (fd gd)

instance Traversable (HashG n) where
  traverse f (HG g gd) = HG <$> traverse (traverse f) g <*> (f gd)

-- Wat do with default value?
-- May need to union the result of 'h gd' with the first part
-- instance (Eq n) => Monad (HashG n) where
--   (HG g gd) >>= h = HG (mapWithKey (\k1 hm -> mapWithKey (\k2 v -> sample (h v) k1 k2) hm) g) ()

-- instance (Eq n, Hashable n, Monoid n) => Comonad (HashG n) where
--   extract g = sample g mempty mempty
--   -- This is probably brutal in space/time
--   -- Have to create HM n (HM n (HM n (HM n e)))
--   -- where lookup dup = lookup (n1 <> n3) (n2 <> n4) g
--   duplicate hg@(HG g gd) = 
--     where
--       hmap = HM.fromList $ filter (/= gd) $
--         (\n1 n2 n3 n4-> (n1, (n2, (n3, (n4, sample g (n1 <> n3) (n2 <> n4)))))
--           <$> ns <*> ns <*> ns <*> ns
--       ns = nodes hg

-- instance Profunctor HashG where
--   dimap ab cd g = HG $ \n1 n2 -> cd (unHG g (ab n1) (ab n2))

--instance Bifunctor HashG where
--  bimap :: (a -> b) -> (c -> d) -> HashG a c -> HashG b d
--  first = ????
--  second = fmap
