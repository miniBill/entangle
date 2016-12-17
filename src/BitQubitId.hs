module BitQubitId (QubitId, BitId, qubitId, toSize) where

newtype QubitId = QubitId Int deriving (Eq, Ord)
newtype BitId = BitId Int deriving (Eq, Ord)

qubitId :: Int -> QubitId
qubitId i
    | i < 0 = error "Negative qubit id"
    | otherwise = QubitId i

bitId :: Int -> BitId
bitId i
    | i < 0 = error "Negative bit id"
    | otherwise = BitId i

liftQubit :: (Int -> Int -> Int) -> QubitId -> QubitId -> QubitId
liftQubit op (QubitId a) (QubitId b) = qubitId $ op a b

instance Show QubitId where
    show (QubitId i) = "qubit " ++ show i

instance Bounded QubitId where
    minBound = QubitId 0
    maxBound = QubitId maxBound

instance Enum QubitId where
    fromEnum (QubitId i) = i
    toEnum = qubitId

instance Num QubitId where
    (+) = liftQubit (+)
    (-) = liftQubit (-)
    fromInteger = qubitId . fromIntegral

instance Show BitId where
    show (BitId i) = "bit " ++ show i

instance Bounded BitId where
    minBound = BitId 0
    maxBound = BitId maxBound

instance Enum BitId where
    fromEnum (BitId i) = i
    toEnum = bitId

toSize :: QubitId -> Integer
toSize (QubitId i) = 2 ^ i
