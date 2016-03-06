type rank = One | Two | Three | Four | Five | Six | Seven | Eight
type file = A | B | C | D | E | F | G | H
type square = Square of file * rank

type descriptive_move = {
  starting_square : square;
  destination_square : square;
}
