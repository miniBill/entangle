module Complex (Complex((:+)), ii) where

-- This module is similar to Data.Complex, but with lighter constraints

infix  6  :+

data Complex a = a :+ a

ii :: Num a => Complex a
ii = 0 :+ 1

instance Num a => Num (Complex a) where
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    (a :+ b) - (c :+ d) = (a - c) :+ (b - d)
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (b * c + a * d)
    fromInteger n = fromInteger n :+ 0
    abs = error "abs not implemented because it would need a Floating instance for the type variable"
    signum = error "signum makes no sense for complex numbers"

instance Fractional a => Fractional (Complex a) where
    recip (a :+ b) =
        let
            d = a * a + b * b
            rp = a / d
            ip = -1 * b / d
        in
            rp :+ ip
    fromRational r = fromRational r :+ 0

instance Floating a => Floating (Complex a) where
    sqrt z =
        let
            r = aabs z
            rc = r :+ 0
            aabs (a :+ b) = sqrt (a * a + b * b)
        in
            (sqrt r :+ 0) * (z + rc) / (aabs (z + rc) :+ 0)

    pi = error "pi"

    log = error "log not implemented"
    exp = error "exp not implemented"

    tan = error "tan not implemented"
    atan = error "atan not implemented"
    atanh = error "atanh not implemented"

    sin = error "sin not implemented"
    asin = error "asin not implemented"
    sinh = error "sinh not implemented"
    asinh = error "asinh not implemented"

    cos = error "cos not implemented"
    acos = error "acos not implemented"
    cosh = error "cosh not implemented"
    acosh = error "acosh not implemented"

instance (Show a, Eq a, Num a) => Show (Complex a) where
    show (a :+ b)
        | b == 0    = show a
        | a == 0    =                  show b ++ "i"
        | otherwise = show a ++ "+" ++ show b ++ "i"
