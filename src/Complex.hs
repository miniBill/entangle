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

instance Show a => Show (Complex a) where
    show (a :+ b) = show a ++ ":+" ++ show b
