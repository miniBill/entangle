{-# LANGUAGE PatternGuards #-}

module SqMath where

import Data.Ratio

data Expr = Leaf Integer
          | Sum Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Sqrt Expr

eval :: Expr -> Either Integer Double
eval (Leaf i) = Left i
eval (Sum i j) = lift2 (+) (+) i j
eval (Mult i j) = lift2 (*) (*) i j
eval (Div i j) = lift2 (div) (/) i j
eval (Abs i) = lift1 (abs) (abs) i
eval (Sqrt i) = (Right . sqrt . to_double . eval) i

to_double :: Either Integer Double -> Double
to_double (Left a) = fromInteger a
to_double (Right b) = b

lift1 :: (Integer -> Integer) -> (Double -> Double) -> Expr -> Either Integer Double
lift1 i d e = lift' (eval e) where
    lift' (Left x) = Left $ i x
    lift' x = Right $ d (to_double x) where

lift2 :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Expr -> Expr -> Either Integer Double
lift2 i d e f = lift' (eval e) (eval f) where
    lift' (Left x) (Left y) = Left $ i x y
    lift' x y = Right $ d (to_double x) (to_double y)

instance Num Expr where
    a + b = simplify $ Sum a b
    a * b = simplify $ Mult a b
    abs a = simplify $ Abs a
    signum = undefined
    fromInteger = Leaf
    negate a = (Leaf (-1)) * a

instance Fractional Expr where
    fromRational r = Leaf (numerator r) / Leaf (numerator r)
    a / b = simplify $ Div a b

instance Floating Expr where
    sqrt a = simplify $ Sqrt a
    pi = undefined
    exp = undefined
    log = undefined
    sin = undefined
    cos = undefined
    asin = undefined
    acos = undefined
    atan = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined

simplify (Sum (Leaf a) (Leaf b)) = Leaf $ a + b
simplify (Sum (Leaf 0) a) = simplify a
simplify (Sum a (Leaf 0)) = simplify a
simplify (Sum (Div a b) c) = (a + b * c) / b
simplify (Sum a (Div b c)) = (a * c + b) / c

simplify (Mult (Leaf 0) _) = 0
simplify (Mult _ (Leaf 0)) = 0
simplify (Mult (Leaf 1) a) = simplify a
simplify (Mult a (Leaf 1)) = simplify a
simplify (Mult (Leaf a) (Leaf b)) = Leaf $ a * b
simplify (Mult (Div a b) c) = (a * c) / b
simplify (Mult a (Div b c)) = (a * b) / c
simplify (Mult (Sqrt a) (Sqrt b)) = sqrt $ a * b
simplify (Mult (Mult a b) c) = a * (b * c)
simplify (Mult a (Leaf b)) = Leaf b * a
simplify (Mult (Leaf a) (Mult (Leaf b) c)) = Leaf (a * b) * c
simplify (Mult a (Mult b@(Leaf _) c)) = b * (a * c)

simplify (Div a s@(Sqrt _)) = (a * s) / (s * s)
simplify (Div (Div a b) c) = a / (b * c)
simplify (Div a (Div b c)) = (a * c) / b
simplify (Div (Leaf a) (Leaf b)) | a `mod` b == 0 = Leaf $ a `div` b
simplify (Div (Mult (Leaf a) b) (Leaf c)) | a `mod` c == 0 = (Leaf $ a `div` c) * b

simplify (Sqrt (Leaf a)) | Just r <- perfectSqrt a = Leaf r

simplify (Abs (Leaf a)) = Leaf $ abs a

simplify e = e

perfectSqrt :: Integer -> Maybe Integer
perfectSqrt i | i < 0 = Nothing
perfectSqrt a | b * b == a = Just b where
    b = floor $ sqrt $ fromInteger a
perfectSqrt _ = Nothing

instance Show Expr where
    showsPrec p e0 = case e0 of
        (Leaf i) -> shows i
        (Sum x y) -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
        (Mult x y) -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
        (Div x y) -> showParen (p >= 7) $ (showsPrec 7 x) . (" / " ++) . (showsPrec 7 y)
        (Abs a) -> ("abs(" ++) . showsPrec 10 a . (++ ")")
        (Sqrt a) -> ("sqrt(" ++) . showsPrec 10 a . (++ ")")
