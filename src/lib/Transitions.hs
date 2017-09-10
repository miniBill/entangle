{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Transitions where

import           Quipper
import           Quipper.Circuit
import           Quipper.Monad

import           BitQubitId
import           Complex
import           EntangleMonad
import           Expr
import qualified GatesMatrices
import           QMatrix
import           QTuple

resetName :: String
resetName = "RESET"

reset :: Qubit -> Circ Qubit
reset = named_gate resetName

{-# ANN module "HLint: ignore Use camelCase" #-}
reset_at :: Qubit -> Circ ()
reset_at = named_gate_at resetName

data StateName = StateName {
    snId :: Integer,  -- ^ state id
    snBs :: [Bool]    -- ^ boolean values in the state
} deriving Eq

instance Ord StateName where
    compare a b
        | snId a < snId b = LT
        | snId a > snId b = GT
        | otherwise = compare' (snBs a) (snBs b) where
            compare' [] []              = EQ
            compare' [] _               = LT
            compare' _ []               = GT
            compare' (True:_) (False:_) = LT
            compare' (False:_) (True:_) = GT
            compare' (_:as) (_:bs)      = compare' as bs

instance Show StateName where
    show (StateName i bs) = show i ++ if null bs then "" else "_" ++ map (\b -> if b then 'T' else 'F') (reverse bs)

data Transitions m v = Transitions {
    trFromState    :: StateName,
    trDestinations :: [Transition m v]
}

instance Show (Transitions m v) where
    show (Transitions from dests) =
        concat
            [ "[Transitions trFromState="
            , show from
            , " trDestinations="
            , show dests
            , "]"
            ]

data Transition m v = Transition {
    trMatrix  :: Maybe (m (Complex v)),
    trToState :: StateName
}

instance Show (Transition m v) where
    show (Transition _ to) = "[Transition trMatrix=... trToState=" ++ show to ++ "]"

type QubitCount = QubitId
type ControlCount = QubitId

-- |circMatrices takes a function returning a value in the 'Circ' monad,
-- and calculates the list of QPMC transitions needed to represent it.
circMatrices :: (FromDouble a, QTuple q, Show b, QCMatrix m a) => (b -> [Transition m a]) -> m a -> (q -> Circ b) -> Either String [Transitions m a]
circMatrices final _ circ = do
    tree <- circToTree circ
    return $ treeToTransitions final tree

circToTree :: QTuple a => (a -> Circ b) -> Either String (CircTree b)
circToTree mcirc = tree where
    arg = tupleFromList $ map qubit_of_wire [1..]
    circ = extract_general arity_empty (mcirc arg)
    argsLength = tupleSize arg
    tree = buildTree circ argsLength

treeToTransitions :: (FromDouble a, Show b, QCMatrix m a) => (b -> [Transition m a]) -> CircTree b -> [Transitions m a]
treeToTransitions final t = go (StateName 0 []) t where
    wires :: [QubitId]
    wires = getWires t
    qubit_max :: QubitId
    qubit_max = foldr max minBound wires
    go sn (LeafNode x) =
        let
            f = final x
        in
            if null f
                then []
                else [Transitions sn f]
    go sn@(StateName i bs) (GateNode name qs cts c) = Transitions sn [tr] : go state' c where
        tr = Transition (Just mat) state'
        mat = gateToMatrixNoParam qubit_max name qs cts
        state' = StateName (succ i) bs
    go sn@(StateName i bs) (ResetNode qi c) =
        let
            state' = StateName (succ $ succ i) bs

            lmat = between (pred qi) (QMatrix.measure UL) (qubit_max - qi)
            ls = StateName (succ i) (bs ++ [True])
            lt = Transition (Just lmat) ls

            rmat = between (pred qi) (QMatrix.measure BR) (qubit_max - qi)
            rs = StateName (succ i) (bs ++ [False])
            rt = Transition (Just rmat) rs

            nmat = between (pred qi) QMatrix.pauliX (qubit_max - qi)
            nt = Transition (Just nmat) state'

            it = Transition Nothing state'
        in
            Transitions sn [lt, rt] : Transitions ls [it] : Transitions rs [nt] : go state' c
    go sn@(StateName i bs) (ParameterizedGateNode name k qs cts c) = Transitions sn [tr] : go state' c where
        tr = Transition (Just mat) state'
        mat = gateToMatrixParameterized qubit_max name k qs cts
        state' = StateName (succ i) bs
    go sn@(StateName i bs) (MeasureNode qi _ l r) = Transitions sn [lt, rt] : go ls l ++ go rs r where
        lmat = between (pred qi) (QMatrix.measure UL) (qubit_max - qi)
        ls = StateName (succ i) (bs ++ [False])
        lt = Transition (Just lmat) ls

        rmat = between (pred qi) (QMatrix.measure BR) (qubit_max - qi)
        rs = StateName (succ i) (bs ++ [True])
        rt = Transition (Just rmat) rs

-- |getWires returns the qubit numbers involved in a gate.
--getWires :: CircTree a -> [QubitId]
getWires :: Show a => CircTree a -> [QubitId]
getWires (LeafNode _)                        = []
getWires (ResetNode q c)                     = q : getWires c
getWires (GateNode _ qs cs c)                = qs ++ cs ++ getWires c
getWires (ParameterizedGateNode _ _ qs cs c) = qs ++ cs ++ getWires c
getWires (MeasureNode q _ l r)               = q : getWires l ++ getWires r

-- |sw q t is a function that swaps q and t
sw :: Eq a => a -> a -> (a -> a)
sw q t x | x == q = t
         | x == t = q
         | otherwise = x

-- |gateToMatrixNoParam takes the total number of qubits, a gate data and returns the matrix needed to represent it.
gateToMatrixNoParam :: QCMatrix m a => QubitCount -> String -> [QubitId] -> [QubitId] -> m (Complex a)
gateToMatrixNoParam size name qs cs =
    let
        active = GatesMatrices.nameToMatrix name
    in
        gateToMatrix size qs cs active

-- |gateToMatrixParameterized takes the total number of qubits, a gate data and returns the matrix needed to represent it.
gateToMatrixParameterized :: (FromDouble a, QCMatrix m a) => QubitCount -> String -> Double -> [QubitId] -> [QubitId] -> m (Complex a)
gateToMatrixParameterized size name t qs cs =
    let
        active = GatesMatrices.nameToMatrixParameterized name t
    in
        gateToMatrix size qs cs active

data SwapType = Multiply | Single

swapType :: SwapType
swapType = Multiply --or Single

-- |gateToMatrix takes the total number of qubits, an active matrix and returns the matrix needed to represent the full gate.
gateToMatrix :: QMatrix m a => QubitCount -> [QubitId] -> [QubitId] -> m a -> m a
gateToMatrix size qs cs active =
    let
        wires = cs ++ qs
        mi = minimum wires
        controlCount = qubitId $ length cs
        qubitCount = qubitId $ length qs
        ma = pred $ mi + controlCount + qubitCount
        l = pred mi
        m = identityPlusMatrix controlCount qubitCount active
        r = size - ma
        mat = between l m r
    in
        case swapType of
            Multiply ->
                let
                    swaps = reverse $ generateSwaps wires [mi..]
                in
                    moving size swaps mat
            Single ->
                let
                    target = [1..(mi-1)] ++ wires ++ [ w | w <- [1..size], w `notElem` wires]
                    swaps = swapToSingleMatrix size target
                in
                    swaps * mat * swaps
bin2dec :: [Bool] -> Int
bin2dec x = undefined

dec2bin :: Int -> Int -> [Bool]
dec2bin x = undefined

swapToSingleMatrix :: QMatrix m a => QubitCount -> [QubitId] -> m a
swapToSingleMatrix size t =
    let
        dim = toSize size
        ddim = downcast dim
        s =
            do
                i <- [1..ddim]
                let origin = dec2bin (i-1) ddim
                let target = [origin !! fromEnum (t !! (j - 1)) | j <- [1..ddim]]
                let c = bin2dec target
                return $ replicate c 0 ++ [1] ++ replicate (ddim - c - 1) 0
        f r c = (s !! downcast r) !! downcast c
    in
        matrix dim dim f
{-

S = zeros(2^length(T))

for i = 1:(2^length(T))
	origin = dec2bin(i-1,length(T)); 	% char array of the original position of the 1
	target = dec2bin(0,length(T));		% char array of the target position of the 1 (now empty)

	% fill the target array doing the actual swap
	for j = 1:length(T)
		target(j) = origin(T(j)+1);
		% (the +1 is necessary since Matlab/Octave works in base 1)
	end % end for

	% put the 1 in the swap matrix in the target-th row
	% (the +1 is necessary since Matlab/Octave works in base 1)
	S(bin2dec(target)+1,i) = 1;


end % end for

-}

-- |generateSwaps takes a finite list of source qubits, a list of target qubits,
-- and returns a list of swaps that moves the qubits into place.
-- Algebrically this is a decomposition of a generic permutation into swaps.
generateSwaps :: Eq t => [t] -> [t] -> [(t, t)]
generateSwaps [] _ = []
generateSwaps _ [] = []
generateSwaps (q:qs) (t:ts)
    | q == t = generateSwaps qs ts
    | otherwise = (q, t) : generateSwaps (map (sw q t) qs) ts

-- |identityPlusMatrix is the matrix composed by putting the given matrix in the bottom right,
-- an identity in the top left and zeroes elsewhere.
identityPlusMatrix :: QMatrix m a => QubitId -> QubitId -> m a -> m a
identityPlusMatrix controlCount qubitCount active =
    let
        total_size = toSize (controlCount + qubitCount)
        small_size = if qubitCount == 0 then 0 else toSize qubitCount
        big_size = total_size - small_size
    in
        if big_size == 0
            then active
            else
                (identity big_size        <|> zero big_size small_size)
                                          <->
                (zero small_size big_size <|> active)

-- |moving returns a matrix representing:
--   * moving the chosen qubits
--   * applying the given matrix
--   * moving the qubits back to their original position
moving :: QMatrix m a => QubitCount -> [(QubitId, QubitId)] -> m a -> m a
moving size moves m = back * m * forth where
    forth = move size moves
    back  = move size $ reverse moves

-- |move is the matrix that moves the chosen qubits
move :: QMatrix m a => QubitCount -> [(QubitId, QubitId)] -> m a
move size = foldr f $ identity (toSize size) where
    f (t1, t2) m = swapToMatrix size t1 t2 * m

-- |swapToMatrix is the matrix swapping the chosen qubits
swapToMatrix :: QMatrix m a => QubitCount -> QubitId -> QubitId -> m a
swapToMatrix size n m
    | n > m = swapToMatrix size m n
    | n == m = identity $ toSize size
    | n < pred m  = swapToMatrix size n (pred m) * swapToMatrix size (pred m) m
    -- otherwise: n == pred m
    | otherwise = between (pred n) QMatrix.swap (size - m)

-- |between takes a matrix and applies it to the chosen qubits,
-- without modifying the other ones
between :: QMatrix m a => QubitCount -> m a -> QubitCount -> m a
between b m a = before `kronecker` m `kronecker` after where
    before = identity $ toSize b
    after  = identity $ toSize a
