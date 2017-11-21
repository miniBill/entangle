
import Quipper

and_gate :: (Qubit, Qubit) -> Circ (Qubit)
and_gate (a, b) = do
  a <- qinit False
  qnot_at a `controlled` [b]
  return a

main =
  print_simple Preview and_gate
