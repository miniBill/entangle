{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  QMonad.MC
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module QMonad.MC (
    MC,
    sc
) where

import QMonad

import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad
import Control.Monad.State.Class

-- Model checker monad
data MC a = MC { runMC :: Map Int String -> (a, Map Int String) }

instance Functor MC where
    fmap = liftM

instance Applicative MC where
    pure  = return
    (<*>) = ap

instance Monad MC where
    return x = MC $ \s -> (x, s)
    (MC f) >>= g = MC res where
        res s0 = result where
            (v, s1) = f s0
            (MC f2) = g v
            result = f2 s1

instance MonadState (Map Int String) MC where
    get = MC (\s -> (s, s))
    put s' = MC (\s -> ((), s'))

instance QMonad MC where
    data Qubit MC = MCQubit Int
    data ControlList MC = MCControlList String
    type EQubit MC = Qubit MC
    embed = id
    umbed = id
    hadamard = liftUnary (\s -> "[Hadamard " ++ s ++ "]")
    qnot = liftUnary (\s -> "[QNot " ++ s ++ "]")

    controlled mq qs = do
        q@(MCQubit i) <- mq
        liftAlter (\s -> "[Controlled " ++ unwords (s : [show i | (MCQubit i) <- qs]) ++ "]") i
        return q

    --with_ancilla :: (Qubit MC -> MC a) -> MC a
    with_ancilla = error "with_ancilla"

    --with_controls :: ControlList MC -> MC a -> MC a
    with_controls = error "with_controls"

    (MCQubit i) .==. b = MCControlList $ "(" ++ show i ++ ".==." ++ show b ++ ")"

    --(.&&.) :: ControlList m -> ControlList m -> ControlList m
    (.&&.) = error ".&&."


liftUnary :: (String -> String) -> Qubit MC -> MC (Qubit MC)
liftUnary f q@(MCQubit i) = do
    liftAlter f i
    return q

liftAlter :: (String -> String) -> Int -> MC ()
liftAlter f i = do
    m <- get
    let m' = alter go i m where
        go = Just . f . may
        may = maybe ("{" ++ show i ++ "}") id
    put m'

class QShow a where
    qshow :: Map Int String -> a -> String

instance (QShow a) => QShow (a, a, a, a, a) where
    qshow m (a, b, c, d, e) = unlines [qshow m e | e <- [a, b, c, d, e]]

instance QShow (Qubit MC) where
    qshow m (MCQubit i) = m ! i

instance (QShow a) => Show (MC a) where
    show (MC f) = unlines (qshow m v : ms) where
        (v, m) = f Map.empty
        ms = [show i ++ ": " ++ e | (i, e) <- toList m]

sc :: Int -> Qubit MC
sc i = MCQubit i