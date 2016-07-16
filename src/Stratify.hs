module Stratify where

import Control.Arrow
import qualified Data.Map.Strict as Map

data StrataState a b = StrataState {
    strataState :: Map.Map b Int,
    composition :: Map.Map Int [a] }

-- |stratify takes as input a list of objects and
-- a function f which for each object extracts a set of numbers
-- and groups them such that:
--
--  * the number of groups is minimal,
--  * the groups are a partition of the input list,
--  * given a group composed of objects [o_1, ..., o_n] we have that
--    the intersection of (f o_i) and (f o_j) is empty iff i =/= j,
--  * if x precedes y in the input list then the group that contains x
--    is the same or precedes the group that contains y.
stratify :: Ord b => (a -> [b]) -> [a] -> [[a]]
stratify f = map snd . Map.toAscList . composition . foldr (stratify' . (id &&& f)) emptyState . reverse where
	emptyState = StrataState Map.empty Map.empty

-- |stratify' is the step the stratification algorithm makes for a single item.
stratify' :: Ord b => (a, [b]) -> StrataState a b -> StrataState a b
stratify' (e, fe) (StrataState strata old) = StrataState newstrata new where
        newstratum = stratum fe strata
        newstrata = foldr (flip Map.insert $ newstratum + 1) strata fe
        new = Map.insertWith (++) newstratum [e] old

-- |stratum returns the group to which the item must be added
-- so that the stratify properties are satisfied.
stratum :: Ord b => [b] -> Map.Map b Int -> Int
stratum x s = foldr (max .  stratum') 0 x where
    stratum' b = Map.findWithDefault 0 b s

-- |strashow shows the result of the stratification algorithm.
strashow :: Show a => [[a]] -> String
strashow xs = foldr (\ x y -> show' x ++ "\n" ++ y) "" $ zip [1..] xs where
    show' (s, es) = show (s :: Int) ++ ": " ++ foldr1 (\e o -> e ++ ", " ++ o) (map show es)
