{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  QMonad
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

module QMonad (
    QMonad(..),
    lift1,
    lift5
) where

class (Monad m) => QMonad m where
    data Qubit m :: *
    data ControlList m :: *
    type EQubit m :: *
    embed :: EQubit m -> Qubit m
    umbed :: Qubit m -> EQubit m
    hadamard :: Qubit m -> m (Qubit m)
    qnot :: Qubit m -> m (Qubit m)
    controlled :: m (Qubit m) -> [Qubit m] -> m (Qubit m)
    with_ancilla :: (Qubit m -> m a) -> m a
    with_controls :: ControlList m -> m a -> m a
    (.==.) :: Qubit m -> Bool -> ControlList m
    infix 4 .==.
    (.&&.) :: ControlList m -> ControlList m -> ControlList m
    infixr 3 .&&.


lift1 :: (QMonad m) => (Qubit m -> m (Qubit m)) -> EQubit m -> m (EQubit m)
lift1 f a = do
    a' <- f $ embed a
    return $ umbed a'

lift5 :: (QMonad m) => (
        Qubit m -> Qubit m -> Qubit m -> Qubit m -> Qubit m ->
        m (Qubit m, Qubit m, Qubit m, Qubit m, Qubit m)) ->
    EQubit m -> EQubit m -> EQubit m -> EQubit m -> EQubit m ->
    m (EQubit m, EQubit m, EQubit m, EQubit m, EQubit m)
lift5 f a b c d e = do
    (a', b', c', d', e') <- f (embed a) (embed b) (embed c) (embed d) (embed e)
    return $ (umbed a', umbed b', umbed c', umbed d', umbed e')
